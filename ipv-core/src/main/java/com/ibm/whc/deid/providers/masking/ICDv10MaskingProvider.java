/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.ICD;
import com.ibm.whc.deid.models.ICDFormat;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.ICDv10MaskingProviderConfig;
import com.ibm.whc.deid.util.ICDv10Manager;
import com.ibm.whc.deid.util.ManagerFactory;

/**
 * The type ICDv10 masking provider.
 *
 */
public class ICDv10MaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -8704305813210528438L;

  protected final boolean generalizeToCategory;
  protected final boolean generalizeToChapter;

  protected transient volatile ICDv10Manager icdv10ResourceManager = null;

  public ICDv10MaskingProvider(ICDv10MaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty, configuration);
    this.generalizeToCategory = configuration.isGeneralizeToCategory();
    this.generalizeToChapter = configuration.isGeneralizeToChapter();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    ICDv10Manager icdv10Manager = getManager();

    ICD icd = icdv10Manager.lookupICD(identifier);

    if (generalizeToChapter || generalizeToCategory) {
      if (icd == null) {
        return applyUnexpectedValueHandling(identifier,
            () -> icdv10Manager.getRandomValue(ICDFormat.CODE));
      }
      ICDFormat format = icd.getFormat();

      if (this.generalizeToChapter) {
        if (format == ICDFormat.CODE) {
          return icd.getChapterCode();
        }
        return icd.getChapterName();
      }

      if (this.generalizeToCategory) {
        if (format == ICDFormat.CODE) {
          return icd.getCategoryCode();
        }
        return icd.getCategoryName();
      }
    }

    ICDFormat targetFormat = icd == null ? ICDFormat.CODE : icd.getFormat();
    return icdv10Manager.getRandomValue(targetFormat);
  }

  protected ICDv10Manager getManager() {
    if (icdv10ResourceManager == null) {
      icdv10ResourceManager = (ICDv10Manager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.ICDV10, null, localizationProperty);
    }
    return icdv10ResourceManager;
  }
}
