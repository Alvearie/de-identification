/*
 * (C) Copyright IBM Corp. 2016,2021
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

  private final boolean randomizeToCategory;
  private final boolean randomizeToRange;
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;

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
    super(tenantId, localizationProperty);
    this.randomizeToCategory = configuration.isRandomizeCategory();
    this.randomizeToRange = configuration.isRandomizeChapter();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    ICDv9Manager icdV9Manager = getManager();

    ICD icd = icdV9Manager.lookupICD(identifier);
    if (icd == null) {
      // TODO: check if ICD is required
      debugFaultyInput("icd");
      if (unspecifiedValueHandling == 2) {
        return icdV9Manager.getRandomKey();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    ICDFormat format = icd.getFormat();

    if (this.randomizeToRange) {
      if (format == ICDFormat.CODE) {
        return icd.getChapterCode();
      }
      return icd.getChapterName();
    }

    if (this.randomizeToCategory) {
      if (format == ICDFormat.CODE) {
        return icd.getCategoryCode();
      }
      return icd.getCategoryName();
    }

    return icdV9Manager.getRandomKey();
  }

  protected ICDv9Manager getManager() {
    if (icdV9ResourceManager == null) {
      icdV9ResourceManager = (ICDv9Manager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.ICDV9, null, localizationProperty);
    }
    return icdV9ResourceManager;
  }
}
