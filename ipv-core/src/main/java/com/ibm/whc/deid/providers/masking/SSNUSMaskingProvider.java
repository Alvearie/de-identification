/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import com.ibm.whc.deid.models.SSNUS;
import com.ibm.whc.deid.providers.identifiers.SSNUSIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.SSNUSMaskingProviderConfig;

public class SSNUSMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 1913380865876971674L;

  private static final SSNUSIdentifier ssnUSIdentifier = new SSNUSIdentifier();

  private final boolean preserveAreaNumber;
  private final boolean preserveGroup;

  /** Instantiates a new Ssnus masking provider. */
  public SSNUSMaskingProvider() {
    this(new SSNUSMaskingProviderConfig());
  }

  /**
   * Instantiates a new Ssnus masking provider.
   *
   * @param configuration the configuration
   */
  public SSNUSMaskingProvider(SSNUSMaskingProviderConfig configuration) {
    super(configuration);
    this.random = new SecureRandom();
    this.preserveAreaNumber = configuration.isMaskPreserveAreaNumber();
    this.preserveGroup = configuration.isMaskPreserveGroup();
  }

  /*
   * The Social Security number is a nine-digit number in the format "AAA-GG-SSSS".[27] The number
   * is divided into three parts. The area number, the first three digits, is assigned by
   * geographical region. The middle two digits are the group number. The last four digits are
   * serial numbers.
   */
  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    String areaNumber = null;
    String group = null;

    if (this.preserveAreaNumber || this.preserveGroup) {
      SSNUS ssn = ssnUSIdentifier.parseSSNUS(identifier);

      if (ssn == null) {
        if (isUnexpectedValueHandlingRandom()) {
          // write the log record, but keep going to get a random value
          debugFaultyInput(identifier);
        } else {
          return applyUnexpectedValueHandling(identifier, null);
        }

      } else {
        if (this.preserveAreaNumber) {
          areaNumber = ssn.getAreaNumber();
        }
        if (this.preserveGroup) {
          group = ssn.getGroup();
        }
      }
    }

    if (areaNumber == null) {
      int areaNumberInt = 0;
      while (areaNumberInt == 0 || areaNumberInt == 666) {
        areaNumberInt = random.nextInt(999);
      }
      areaNumber = String.format("%03d", areaNumberInt);
    }

    if (group == null) {
      group = String.format("%02d", random.nextInt(99));
    }

    String serialNumber = String.format("%04d", random.nextInt(9999));

    return (new SSNUS(areaNumber, group, serialNumber)).toString();
  }
}
