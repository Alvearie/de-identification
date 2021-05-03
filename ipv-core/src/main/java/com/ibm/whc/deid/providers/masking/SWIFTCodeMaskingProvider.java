/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import com.ibm.whc.deid.models.SWIFTCode;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.SWIFTMaskingProviderConfig;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.SWIFTCodeManager;

public class SWIFTCodeMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -6934133173548691359L;

  protected transient volatile SWIFTCodeManager swiftCodeResourceManager = null;

  protected final boolean preserveCountry;

  /**
   * Instantiates a new SWIFT masking provider.
   * 
   * @param configuration the provider configuration
   * @param tenantId
   * @param localizationProperty
   */
  public SWIFTCodeMaskingProvider(SWIFTMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty, configuration);
    this.preserveCountry = configuration.isPreserveCountry();
    this.random = new SecureRandom();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    SWIFTCodeManager swiftCodeManager = getSWIFTCodeManager();

    String code = null;
    String countryCode = null;

    if (this.preserveCountry) {
      // need a valid input code from which to extract the country
      try { 
        SWIFTCode inputSwiftCode = new SWIFTCode(identifier);
        countryCode = inputSwiftCode.getCountry();
        code = swiftCodeManager.getRandomValueFromCountry(countryCode);

      } catch (IllegalArgumentException e) {
        // not a valid format SWIFT code
        if (!isUnexpectedValueHandlingRandom()) {
          // if random, fall through to generate a random code
          return applyUnexpectedValueHandling(identifier, null);
        }
      }
    }

    if (code == null) {
      // not preserving country OR
      // input isn't valid, but handling requests a random value OR
      // no codes for the associated country are loaded
      // so get a random value from the loaded codes
      SWIFTCode swiftCode = swiftCodeManager.getRandomValue();
      if (swiftCode != null) {
        code = swiftCode.getCode();
      }

      if (code == null) {
        // no codes are loaded, generate a random one
        // codes are 8 or 11 characters long
        int length = this.random.nextBoolean() ? 11 : 8;
        StringBuilder buffer = new StringBuilder(length);
        for (int i = 0; i < 4; i++) {
          buffer.append(getSwiftCodeChar(false));
        }
        if (countryCode == null) {
          // country code isn't known, just generate a random one
          for (int i = 4; i < 6; i++) {
            buffer.append(getSwiftCodeChar(false));
          }
        } else {
          // country code is known and only possible if trying to preserve country,
          // so use it in the randomly-generated value
          buffer.append(countryCode);
        }
        for (int i = 6; i < length; i++) {
          buffer.append(getSwiftCodeChar(true));
        }
        code = buffer.toString();
      }
    }

    return code;
  }

  protected char getSwiftCodeChar(boolean allowDigit) {
    char ch;
    int index = this.random.nextInt(allowDigit ? 36 : 26);
    if (index >= 26) {
      ch = (char) ('0' + (index - 26));
    } else {
      ch = (char) ('A' + index);
    }
    return ch;
  }

  protected SWIFTCodeManager getSWIFTCodeManager() {
    if (swiftCodeResourceManager == null) {
      swiftCodeResourceManager = (SWIFTCodeManager) ManagerFactory.getInstance()
          .getManager(tenantId, Resource.SWIFT, null, localizationProperty);
    }
    return swiftCodeResourceManager;
  }
}
