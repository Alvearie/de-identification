/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import org.apache.commons.lang3.SerializationUtils;
import org.junit.Test;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.HashMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.NameMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.ConfigGenerator;

public class MaskingProviderFactoryTest {

  private final String tenantId = "TEST_TENANT";

  @Test
  public void maskNamedFields() {

    Map<String, ProviderType> identifiedTypes = new HashMap<>();
    identifiedTypes.put("name", ProviderType.NAME);
    DeidMaskingConfig deidMaskingConfig = new DeidMaskingConfig();

    /*
     * Instantiate a MaskingProviderFactory with a ConfigurationManager and the map of identified
     * types
     */
    MaskingProviderFactory maskingProviderFactory = new BasicMaskingProviderFactory(
        new DeidMaskingConfig(), identifiedTypes);

    /* Get the masking provider and mask a value */
    MaskingProvider maskingProvider = maskingProviderFactory.getProviderFromType(
        MaskingProviderType.HASH, deidMaskingConfig, new HashMaskingProviderConfig(), tenantId);

    String originalValue = "John Smith";
    String maskedValue = maskingProvider.mask(originalValue);
    assertNotEquals(maskedValue, originalValue);
  }

  @Test
  public void testWithoutIdentified() {
    DeidMaskingConfig deidMaskingConfig = new DeidMaskingConfig();

    MaskingProviderFactory maskingProviderFactory =
        new BasicMaskingProviderFactory(deidMaskingConfig, null);
    MaskingProvider maskingProvider = maskingProviderFactory.getProviderFromType(
        MaskingProviderType.HASH, deidMaskingConfig, new HashMaskingProviderConfig(), tenantId);

    String originalValue = "John Smith";
    String maskedValue = maskingProvider.mask(originalValue);
    assertNotEquals(maskedValue, originalValue);
  }

  @Test
  public void testPersistent() {
    MaskingProviderFactory maskingProviderFactory = new BasicMaskingProviderFactory();
    DeidMaskingConfig deidMaskingConfig = new DeidMaskingConfig();

    MaskingProvider maskingProvider = maskingProviderFactory.getProviderFromType(
        MaskingProviderType.NAME, deidMaskingConfig, new NameMaskingProviderConfig(), tenantId);

    assertTrue(maskingProvider instanceof NameMaskingProvider);
  }

  @Test
  public void verifySerialization() throws Exception {
    MaskingProviderFactory mpf = new BasicMaskingProviderFactory();

    // Construct from a configuration manager so that wrapping providers can
    // instantiate other providers if needed

    String tenantId = "testTenant";
    DeidMaskingConfig deidMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();

    // The main masking provider FHIR/GEN do not have
    // MaskingProviderConfig of their own
    MaskingProviderType typesToIgnore[] =
        {MaskingProviderType.FHIR, MaskingProviderType.GEN};
    List<MaskingProviderType> list = Arrays.asList(typesToIgnore);

    Stream.of(MaskingProviderType.values()).filter(type -> !list.contains(type))
        .map(providerType -> {

          try {
            MaskingProviderConfig config =
                MaskingProviderConfig.getDefaultMaskingProviderConfig(providerType);
            MaskingProvider maskingProvider =
                mpf.getProviderFromType(providerType, deidMaskingConfig, config, tenantId);

            assertNotNull(maskingProvider);
            assertTrue(maskingProvider instanceof Serializable);
            try {
              assertNotSame(maskingProvider, SerializationUtils.clone(maskingProvider));
            } catch (Exception e) {
              e.printStackTrace();
            }
          } catch (IllegalArgumentException e) {
            // Ignore illegal argument exception if we cannot instantiate a masking provider
            // because we do not recognize it
          }

          return null;
        }).count();
  }
}
