/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import com.ibm.whc.deid.models.SWIFTCode;
import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.providers.masking.SWIFTCodeMaskingProvider;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.SWIFTCodeManager;

public class SWIFTCodeIdentifier extends AbstractManagerBasedIdentifier {

  private static final long serialVersionUID = -2241208945295394974L;

  private static final String[] appropriateNames = {"SWIFT"};

  protected transient volatile SWIFTCodeManager swiftCodeManager = null;

  public SWIFTCodeIdentifier(String tenantId, String localizationProperty) {
    super(tenantId, localizationProperty);
  }

  @Override
  protected Manager getManager() {
    if (swiftCodeManager == null) {
      swiftCodeManager = (SWIFTCodeManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.SWIFT, null, localizationProperty);
    }
    return swiftCodeManager;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }

  @Override
  public ProviderType getType() {
    return ProviderType.SWIFT;
  }

  @Override
  public boolean isOfThisType(String identifier) {
    boolean valid;
    if (identifier == null) {
      valid = false;
    } else {
      // commonly, no codes are loaded in the manager
      // if any codes are loaded, use the loaded codes to recognize input
      // if not, use the regular expression
      List<SWIFTCode> values = ((SWIFTCodeManager) getManager()).getValues();
      if (values != null && !values.isEmpty()) {
        valid = super.isOfThisType(identifier);
      } else {
        valid =
            SWIFTCodeMaskingProvider.SWIFTCODE_PATTERN.matcher(identifier.toUpperCase()).matches();
      }
    }
    return valid;
  }

  @Override
  public String getDescription() {
    return "SWIFT Code identification";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }
}
