/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import com.ibm.whc.deid.configuration.MaskingConfiguration;
import com.ibm.whc.deid.models.SSNUS;
import com.ibm.whc.deid.providers.identifiers.SSNUSIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.SSNUSMaskingProviderConfig;

public class SSNUSMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = 1913380865876971674L;

  private static final SSNUSIdentifier ssnUSIdentifier = new SSNUSIdentifier();

  private final boolean preserveAreaNumber;
  private final boolean preserveGroup;
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;

  /** Instantiates a new Ssnus masking provider. */
  public SSNUSMaskingProvider() {
    this(new SSNUSMaskingProviderConfig());
  }

  /**
   * Instantiates a new Ssnus masking provider.
   *
   * @param configuration the configuration
   */
  public SSNUSMaskingProvider(MaskingConfiguration configuration) {
    this(new SecureRandom(), configuration);
  }

  /**
   * Instantiates a new Ssnus masking provider.
   *
   * @param random the random
   * @param configuration the configuration
   */
  public SSNUSMaskingProvider(SecureRandom random, MaskingConfiguration configuration) {
    this.random = random;
    this.preserveAreaNumber = configuration.getBooleanValue("ssnus.mask.preserveAreaNumber");
    this.preserveGroup = configuration.getBooleanValue("ssnus.mask.preserveGroup");
    this.unspecifiedValueHandling = configuration.getIntValue("unspecified.value.handling");
    this.unspecifiedValueReturnMessage =
        configuration.getStringValue("unspecified.value.returnMessage");
  }

  public SSNUSMaskingProvider(SSNUSMaskingProviderConfig configuration) {
    this.random = new SecureRandom();
    this.preserveAreaNumber = configuration.isMaskPreserveAreaNumber();
    this.preserveGroup = configuration.isMaskPreserveGroup();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  /*
   * The Social Security number is a nine-digit number in the format "AAA-GG-SSSS".[27] The number
   * is divided into three parts. The area number, the first three digits, is assigned by
   * geographical region. The middle two digits are the group number The last four digits are serial
   * numbers
   */
  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    SSNUS ssn = ssnUSIdentifier.parseSSNUS(identifier);
    String areaNumber;
    String group;
    String serialNumber = String.format("%04d", random.nextInt(9999));

    if (ssn != null) {
      if (this.preserveAreaNumber) {
        areaNumber = ssn.getAreaNumber();
      } else {
        int areaNumberInt = 0;
        while (areaNumberInt == 0 || areaNumberInt == 666) {
          areaNumberInt = random.nextInt(999);
        }
        areaNumber = String.format("%03d", areaNumberInt);
      }

      if (this.preserveGroup) {
        group = ssn.getGroup();
      } else {
        group = String.format("%02d", random.nextInt(99));
      }
    } else {
      debugFaultyInput("ssn");
      if (unspecifiedValueHandling == 2) {
        areaNumber = String.format("%03d", random.nextInt(999));
        group = String.format("%02d", random.nextInt(99));
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    return (new SSNUS(areaNumber, group, serialNumber)).toString();
  }
}
