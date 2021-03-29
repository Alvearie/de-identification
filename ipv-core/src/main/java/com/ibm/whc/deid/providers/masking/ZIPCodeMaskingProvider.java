/*
 * (C) Copyright IBM Corp. 2016,2020
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
  /** */
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
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;
  protected ZIPCodeManager zipCodeManager;
  protected PostalCodeManager postalCodeManager;

  protected volatile boolean initialized = false;

  public ZIPCodeMaskingProvider(ZIPCodeMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty);
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
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();

    this.random = new SecureRandom();
  }

  @Override
  public String mask(String identifier) {
    initialize();

    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    if (identifier.length() != zipCodeManager.getZipCodeLength(this.countryCode)) {
      debugFaultyInput("identifier");
      if (unspecifiedValueHandling == 2) {
        return zipCodeManager.getRandomKey();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    // Check if ZIP code should be randomly replaced by a neighboring
    // ZIP code
    if (replaceWithNeighbor && replaceWithNeighborNearestCount > 0) {
      List<PostalCode> nearest =
          postalCodeManager.getClosestPostalCodes(identifier, replaceWithNeighborNearestCount + 1);
      if (nearest.size() > 0) {
        identifier = nearest.get(random.nextInt(nearest.size())).getName();
      } else {
        identifier = postalCodeManager.getRandomKey();
      }
    }

    // Check if total population of ZIP codes with the same prefix must
    // reach a minimum value, if not, then replace prefix with zeros
    if (prefixRequireMinPopulation || truncateIfNotMinPopulation) {

      if (identifier.length() >= prefixLength) {
        String prefix = identifier.substring(0, prefixLength);
        String suffix = identifier.substring(prefixLength);
        Integer population = zipCodeManager.getPopulationByPrefix(countryCode, prefix);
        if (population == null || population < prefixMinPopulation) {
          if (prefixRequireMinPopulation) {
            identifier = StringUtils.leftPad(suffix, prefixLength + suffix.length(), '0');
          }

          // Check if truncate ZIP code if total population of ZIP
          // codes with the same prefix must reach a minimum value
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

        // Check if truncate ZIP code if total population of ZIP
        // codes with the same prefix must reach a minimum value
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

        // Check if suffix must be replaced to create a valid zip
        // code
        if (suffixReplaceWithValidOnly) {
          String randomZip = zipCodeManager.getRandomZipCodeByPrefix(countryCode, identifier);
          if (randomZip != null) {
            identifier = randomZip;
          }
        } else {
          String prefix = identifier.substring(0, prefixLength);
          String suffix = identifier.substring(prefixLength);
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

  protected void initialize() {
    if (!initialized) {
      postalCodeManager = (PostalCodeManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.POSTAL_CODES, null, localizationProperty);

      zipCodeManager = (ZIPCodeManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.ZIPCODE, prefixLength, localizationProperty);

      initialized = true;
    }
  }
}
