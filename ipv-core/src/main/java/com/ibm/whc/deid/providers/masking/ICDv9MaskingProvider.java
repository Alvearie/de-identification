/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.ICD;
import com.ibm.whc.deid.models.ICDFormat;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.ICDv9MaskingProviderConfig;
import com.ibm.whc.deid.util.ICDv9Manager;
import com.ibm.whc.deid.util.ManagerFactory;


/**
 * Privacy provider for ICDv9 codes.
 */
public class ICDv9MaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -5706336758492457693L;

  private final boolean generalizeToCategory;
  private final boolean generalizeToChapter;

  protected transient volatile ICDv9Manager icdV9ResourceManager = null;

  /**
   * Instantiates a new masking provider.
   *
   * @param configuration the provider configuration
   * @param tenantId tenant associated with the current request
   * @param localizationProperty location of the localization property file
   */
  public ICDv9MaskingProvider(ICDv9MaskingProviderConfig configuration, String tenantId,
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

    ICDv9Manager icdV9Manager = getManager();

    ICD icd = icdV9Manager.lookupICD(identifier);

    if (generalizeToChapter || generalizeToCategory) {
      if (icd == null) {
        return applyUnexpectedValueHandling(identifier,
            () -> icdV9Manager.getRandomValue(ICDFormat.CODE));
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
    return icdV9Manager.getRandomValue(targetFormat);
  }

  protected ICDv9Manager getManager() {
    if (icdV9ResourceManager == null) {
      icdV9ResourceManager = (ICDv9Manager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.ICDV9, null, localizationProperty);
    }
    return icdV9ResourceManager;
  }
}
