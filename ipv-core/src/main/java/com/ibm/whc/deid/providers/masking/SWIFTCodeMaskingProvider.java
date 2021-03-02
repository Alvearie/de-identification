/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import java.util.regex.Pattern;
import com.ibm.whc.deid.models.SWIFTCode;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.SWIFTMaskingProviderConfig;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.SWIFTCodeManager;

public class SWIFTCodeMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -6934133173548691359L;

  // country code is offsets 4 and 5
  protected static final Pattern SWIFTCODE_PATTERN = Pattern.compile("[A-Z]{6}[A-Z0-9]{2,5}");

  protected transient volatile SWIFTCodeManager swiftCodeManager = null;

  protected final boolean preserveCountry;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  /**
   * Instantiates a new SWIFT masking provider.
   * 
   * @param configuration the provider configuration
   * @param tenantId
   * @param localizationProperty
   */
  public SWIFTCodeMaskingProvider(SWIFTMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty);
    this.preserveCountry = configuration.isPreserveCountry();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
    this.random = new SecureRandom();
  }

  @Override
  public String mask(String identifier) {
    initialize();

    String code = null;
    String countryCode = null;

    if (this.preserveCountry) {

      if (identifier != null) {
        identifier = identifier.toUpperCase();
        if (SWIFTCODE_PATTERN.matcher(identifier).matches()) {
          countryCode = identifier.substring(4, 6);
        }
      }

      if (countryCode == null) {
        if (unspecifiedValueHandling == 3) {
          return unspecifiedValueReturnMessage;
        } else if (unspecifiedValueHandling != 2) {
          return null;
        }

      } else {
        code = swiftCodeManager.getRandomCodeFromCountry(countryCode);
      }
    }

    if (code == null) {
      SWIFTCode swiftCode = swiftCodeManager.getRandomValue();
      if (swiftCode != null) {
        code = swiftCode.getCode();
      }

      if (code == null) {
        // no codes are loaded, generate a random one
        int length = this.random.nextBoolean() ? 11 : 8;
        StringBuilder buffer = new StringBuilder(length);
        for (int i = 0; i < 4; i++) {
          buffer.append(getSwiftCodeChar(false));
        }
        if (countryCode == null) {
          for (int i = 4; i < 6; i++) {
            buffer.append(getSwiftCodeChar(false));
          }
        } else {
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

  protected void initialize() {
    if (swiftCodeManager == null) {
      synchronized (this) {
        if (swiftCodeManager == null) {
          swiftCodeManager = (SWIFTCodeManager) ManagerFactory.getInstance().getManager(tenantId,
              Resource.SWIFT, null, localizationProperty);
        }
      }
    }
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
}
