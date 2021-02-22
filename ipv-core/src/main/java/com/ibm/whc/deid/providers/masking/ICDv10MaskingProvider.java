/*
 * (C) Copyright IBM Corp. 2016,2020
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
 * The type Ic dv 10 masking provider.
 *
 */
public class ICDv10MaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -8704305813210528438L;

  protected ICDv10Manager icdv10Manager;
  protected final boolean randomizeToCategory;
  protected final boolean randomizeToRange;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  protected volatile boolean initialized = false;

  public ICDv10MaskingProvider(ICDv10MaskingProviderConfig configuration, String tenantId) {
    this.randomizeToCategory = configuration.isRandomizeCategory();
    this.randomizeToRange = configuration.isRandomizeChapter();

    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  @Override
  public String mask(String identifier) {
    initialize();
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

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
      } else {
        return icd.getChapterName();
      }
    } else if (this.randomizeToCategory) {
      if (format == ICDFormat.CODE) {
        return icd.getCategoryCode();
      } else {
        return icd.getCategoryName();
      }
    }

    return icdv10Manager.getRandomKey();
  }

  protected void initialize() {
    if (!initialized) {
      icdv10Manager =
          (ICDv10Manager) ManagerFactory.getInstance().getManager(null, Resource.ICDV10, null, localizationProperty);
      initialized = true;
    }
  }

}
