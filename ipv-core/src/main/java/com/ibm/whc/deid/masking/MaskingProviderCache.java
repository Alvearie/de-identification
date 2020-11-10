/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.masking;

import java.io.IOException;
import java.util.concurrent.TimeUnit;
import org.apache.commons.codec.digest.DigestUtils;
import org.ehcache.Cache;
import org.ehcache.CacheManager;
import org.ehcache.config.builders.CacheConfigurationBuilder;
import org.ehcache.config.builders.CacheManagerBuilder;
import org.ehcache.config.builders.ResourcePoolsBuilder;
import org.ehcache.expiry.Duration;
import org.ehcache.expiry.Expirations;
import com.ibm.whc.deid.providers.masking.BasicMaskingProviderFactory;
import com.ibm.whc.deid.providers.masking.ComplexMaskingProviderFactory;
import com.ibm.whc.deid.providers.masking.ComplexMaskingProviderFactoryUtil;
import com.ibm.whc.deid.providers.masking.MaskingProvider;
import com.ibm.whc.deid.providers.masking.MaskingProviderFactoryUtil;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import com.ibm.whc.deid.shared.util.MaskingConfigUtils;

/**
 * Cache masking providers. Masking providers may take a while to instantiate since some of them
 * need to load CSV from files or databases. So we cache masking providers.
 *
 * <p>
 * This class creates a cache for each tenant. If a tenant uploads new CSV files, we invalidate the
 * cache for that tenant. We also set TIME TO IDLE to be 5 min. Because the endpoint may be running
 * in multiple pods and there is a cache per endpoint, if one endpoint receives the request to
 * upload CSV, we should invalidate the cache for all endpoints, which we can't do. To mitigate the
 * issue, set the TIME TO IDLE to be 5 min. So the user just need to wait for 5 min after uploading
 * CSV to make sure they use the latest version.
 *
 */
public class MaskingProviderCache {

  protected final ComplexMaskingProviderFactory complexMaskingProviderFactory;

  protected final CacheManager cacheManager;

  protected final long TIME_TO_IDLE_MIN = 5;

  protected final int HEAP_POOL_SIZE = 10;

  public MaskingProviderCache() {

    complexMaskingProviderFactory =
        ComplexMaskingProviderFactoryUtil.getComplexMaskingProviderFactory();

    cacheManager = CacheManagerBuilder.newCacheManagerBuilder().build();
    cacheManager.init();
  }

  /**
   * Get the cache name for the per tenant cache.
   *
   * @param tenantId
   * @return
   */
  private String getCacheName(String tenantId) {
    return "MaskingProvider_" + tenantId;
  }

  protected Cache<String, MaskingProvider> getPerTenantCache(String tenantId) {
    Cache<String, MaskingProvider> perTenantCache;

    String cacheName = getCacheName(tenantId);

    perTenantCache = cacheManager.getCache(cacheName, String.class, MaskingProvider.class);

    if (perTenantCache == null) {
      synchronized (cacheManager) {
        // Multiple threads might have received null from the non-synchronized check, so
        // verify in the synchronized block that the cache doesn't exist before creating it.
        // It is an error to create a cache with a name of an existing cache.

        perTenantCache = cacheManager.getCache(cacheName, String.class, MaskingProvider.class);

        if (perTenantCache == null) {
          perTenantCache = cacheManager.createCache(cacheName,
              CacheConfigurationBuilder
                  .newCacheConfigurationBuilder(String.class, MaskingProvider.class,
                      ResourcePoolsBuilder.heap(HEAP_POOL_SIZE))
                  .withExpiry(Expirations
                      .timeToIdleExpiration(Duration.of(TIME_TO_IDLE_MIN, TimeUnit.MINUTES))));
        }
      }
    }

    return perTenantCache;
  }

  /**
   * Get a MaskingProvider with the given configuration. Return a cached object if available. Use
   * MD5 checksum of the configuration as the key for the cache
   *
   * @param configuration
   * @param tenantId
   * @param schemaType
   * @return
   * @throws DeidException
   * @throws IOException
   */
  public MaskingProvider getMaskingProvider(String configuration, String tenantId,
      ConfigSchemaTypes schemaType)
      throws DeidException, IOException {
    MaskingProvider complexMaskingProvider = null;
    Cache<String, MaskingProvider> perTenantCache = null;

    // Get the md5checksum of the configuration
    String md5Checksum = DigestUtils.md5Hex(configuration);
    if (tenantId != null) {
      perTenantCache = getPerTenantCache(tenantId);
      complexMaskingProvider = perTenantCache.get(md5Checksum);
    }

    if (complexMaskingProvider == null) {
      DeidMaskingConfig deidMaskingConfig;
      try {
        deidMaskingConfig = MaskingConfigUtils.validateConfig(configuration);
      } catch (InvalidMaskingConfigurationException e) {
        throw new DeidException(e.getMessage(), e);
      }

      BasicMaskingProviderFactory maskingProviderFactory =
          (BasicMaskingProviderFactory) MaskingProviderFactoryUtil
              .getNewMaskingProviderFactory(deidMaskingConfig, null);
      complexMaskingProvider = complexMaskingProviderFactory.get(schemaType, deidMaskingConfig,
          maskingProviderFactory, tenantId);
      if (perTenantCache != null) {
        perTenantCache.put(md5Checksum, complexMaskingProvider);
      }
    }

    return complexMaskingProvider;
  }

  /**
   * Remove the cache for a particular tenant.
   *
   * @param tenantId
   */
  public void removeCache(String tenantId) {
    cacheManager.removeCache(getCacheName(tenantId));
  }
}
