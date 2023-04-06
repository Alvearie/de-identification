/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import java.util.Arrays;
import java.util.Collection;
import com.ibm.whc.deid.models.ICD;
import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.ICDv10Manager;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;

/**
 * The type ICDv10 Identifier.
 *
 */
public class ICDv10Identifier extends AbstractManagerBasedIdentifier {

  private static final long serialVersionUID = -7484093125257873714L;

  private static final String[] appropriateNames = {"ICD", "Disease code", "ICD10", "ICDv10"};

  protected transient volatile ICDv10Manager icdv10Manager = null;

  public ICDv10Identifier(String tenantId, String localizationProperty) {
    super(tenantId, localizationProperty);
  }

  @Override
  public ProviderType getType() {
    return ProviderType.ICDv10;
  }

  @Override
  public String getDescription() {
    return "ICD-10 identification";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }

  @Override
  protected Manager getManager() {
    if (icdv10Manager == null) {
      icdv10Manager = (ICDv10Manager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.ICDV10, null, localizationProperty);
    }
    return icdv10Manager;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
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
      if (((ICDv10Manager) getManager()).hasValues()) {
        valid = super.isOfThisType(identifier);
      } else {
        valid = ICD.ICDV10_PATTERN.matcher(identifier.toUpperCase()).matches();
      }
    }
    return valid;
  }
}
