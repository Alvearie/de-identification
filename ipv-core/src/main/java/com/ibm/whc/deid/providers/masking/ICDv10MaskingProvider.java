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

  protected final boolean randomizeToCategory;
  protected final boolean randomizeToRange;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  protected transient volatile ICDv10Manager icdv10ResourceManager = null;

  public ICDv10MaskingProvider(ICDv10MaskingProviderConfig configuration, String tenantId,
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

    ICDv10Manager icdv10Manager = getManager();

    ICD icd = icdv10Manager.lookupICD(identifier);

    if (icd == null) {
      // TODO: check is icd is required
      debugFaultyInput("icd");
      if (unspecifiedValueHandling == 2) {
        return icdv10Manager.getRandomKey();
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

    return icdv10Manager.getRandomKey();
  }

  protected ICDv10Manager getManager() {
    if (icdv10ResourceManager == null) {
      icdv10ResourceManager = (ICDv10Manager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.ICDV10, null, localizationProperty);
    }
    return icdv10ResourceManager;
  }
}
