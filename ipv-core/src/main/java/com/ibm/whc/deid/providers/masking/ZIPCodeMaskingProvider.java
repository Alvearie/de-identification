/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import java.util.List;
import org.apache.commons.lang.StringUtils;
import com.ibm.whc.deid.models.PostalCode;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.ZIPCodeMaskingProviderConfig;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.PostalCodeManager;
import com.ibm.whc.deid.util.RandomGenerators;
import com.ibm.whc.deid.util.ZIPCodeManager;

public class ZIPCodeMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 2186695347144836127L;

  private final String countryCode;
  private final boolean replaceWithNeighbor;
  private final int replaceWithNeighborNearestCount;
  protected final int prefixLength;
  private final boolean prefixRequireMinPopulation;
  private final int prefixMinPopulation;
  private final boolean truncateIfNotMinPopulation;
  private final int truncateLengthIfNotMinPopulation;
  private final boolean suffixTruncate;
  private final boolean suffixReplaceWithRandom;
  private final boolean suffixReplaceWithValidOnly;

  protected transient volatile ZIPCodeManager zipCodeResourceManager = null;
  protected transient volatile PostalCodeManager postalCodeResourceManager = null;

  public ZIPCodeMaskingProvider(ZIPCodeMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty, configuration);
    this.countryCode = configuration.getMaskCountryCode();
    this.replaceWithNeighbor = configuration.isMaskReplaceWithNeighbor();
    this.replaceWithNeighborNearestCount = configuration.getMaskReplaceWithNeighborNearestCount();
    this.prefixLength = configuration.getMaskPrefixLength();
    this.prefixRequireMinPopulation = configuration.isMaskPrefixRequireMinPopulation();
    this.prefixMinPopulation = configuration.getMaskPrefixMinPopulation();
    this.truncateIfNotMinPopulation = configuration.isMaskTruncateIfNotMinPopulation();
    this.truncateLengthIfNotMinPopulation = configuration.getMaskTruncateLengthIfNotMinPopulation();
    this.suffixTruncate = configuration.isMaskSuffixTruncate();
    this.suffixReplaceWithRandom = configuration.isMaskSuffixReplaceWithRandom();
    this.suffixReplaceWithValidOnly = configuration.isMaskSuffixReplaceWithValidOnly();
    this.random = new SecureRandom();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    ZIPCodeManager zipCodeManager = getZIPCodeManager();

    if (identifier.length() != zipCodeManager.getZipCodeLength(countryCode)) {
      return applyUnexpectedValueHandling(identifier, () -> {
        String key = zipCodeManager.getRandomKey(countryCode);
        return key == null ? zipCodeManager.getRandomKey() : key;
      });
    }

    // Check if ZIP code should be randomly replaced by a neighboring
    // ZIP code
    if (replaceWithNeighbor && replaceWithNeighborNearestCount > 0) {
      PostalCodeManager postalCodeManager = getPostalCodeManager();
      List<PostalCode> nearest =
          postalCodeManager.getClosestPostalCodes(identifier, replaceWithNeighborNearestCount);
      if (nearest.size() > 0) {
        identifier = nearest.get(random.nextInt(nearest.size())).getName();
      } else {
        identifier = postalCodeManager.getRandomKey();
      }
    }

    // Check if total population of ZIP codes with the same prefix must
    // reach a minimum value, and if not, then replace prefix with zeros
    if (prefixRequireMinPopulation || truncateIfNotMinPopulation) {

      if (identifier.length() >= prefixLength) {
        String prefix = identifier.substring(0, prefixLength);
        String suffix = identifier.substring(prefixLength);
        Integer population =
            zipCodeManager.getPopulationByPrefix(countryCode, prefix);
        if (population == null || population.intValue() < prefixMinPopulation) {

          if (prefixRequireMinPopulation) {
            identifier = StringUtils.leftPad(suffix, prefixLength + suffix.length(), '0');
          }

          if (truncateIfNotMinPopulation) {
            if (identifier.length() > truncateLengthIfNotMinPopulation) {
              identifier = identifier.substring(0, truncateLengthIfNotMinPopulation);
            }
          }
        }

      } else if (prefixMinPopulation > 0) {

        if (prefixRequireMinPopulation) {
          identifier = StringUtils.leftPad("", prefixLength, '0');
        }

        if (truncateIfNotMinPopulation) {
          if (identifier.length() > truncateLengthIfNotMinPopulation) {
            identifier = identifier.substring(0, truncateLengthIfNotMinPopulation);
          }
        }
      }
    }

    // Check if the suffix is being replaced
    if (suffixReplaceWithRandom) {
      if (identifier.length() > prefixLength) {
        String prefix = identifier.substring(0, prefixLength);
        String suffix = identifier.substring(prefixLength);

        // Check if suffix must be replaced to create a valid zipcode
        if (suffixReplaceWithValidOnly) {
          String randomZip = zipCodeManager.getRandomZipCodeByPrefix(countryCode, prefix);
          if (randomZip != null) {
            identifier = randomZip;
          } else {
            return applyUnexpectedValueHandling(identifier, () -> {
              String key = zipCodeManager.getRandomKey(countryCode);
              return key == null ? zipCodeManager.getRandomKey() : key;
            });
          }
        } else {
          identifier = prefix + RandomGenerators.randomReplacement(suffix);
        }
      }
    }

    // Check if the suffix is being truncated
    if (suffixTruncate) {
      if (identifier.length() > prefixLength) {
        identifier = identifier.substring(0, prefixLength);
      }
    }

    return identifier;
  }

  protected PostalCodeManager getPostalCodeManager() {
    if (postalCodeResourceManager == null) {
      postalCodeResourceManager = (PostalCodeManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.POSTAL_CODES, null, localizationProperty);
    }
    return postalCodeResourceManager;
  }

  protected ZIPCodeManager getZIPCodeManager() {
    if (zipCodeResourceManager == null) {
      zipCodeResourceManager = (ZIPCodeManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.ZIPCODE, null, localizationProperty);
    }
    return zipCodeResourceManager;
  }
}
