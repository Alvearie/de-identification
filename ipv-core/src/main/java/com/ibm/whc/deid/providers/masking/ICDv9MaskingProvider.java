/*
 * (C) Copyright IBM Corp. 2016,2020
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
 * The type Ic dv 9 masking provider.
 *
 */
public class ICDv9MaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -5706336758492457693L;

  protected ICDv9Manager icdV9Manager;
  private final boolean randomizeToCategory;
  private final boolean randomizeToRange;
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;

  protected volatile boolean initialized = false;

  /**
   * Instantiates a new Ic dv 9 masking provider.
   *
   * @param configuration the configuration
   * @param tenantId tenant id
   * @param random the random
   */
  public ICDv9MaskingProvider(ICDv9MaskingProviderConfig configuration, String tenantId) {
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

    return icdV9Manager.getRandomKey();
  }

  protected void initialize() {
    if (!initialized) {
      icdV9Manager =
          (ICDv9Manager) ManagerFactory.getInstance().getManager(null, Resource.ICDV9, null);
      initialized = true;
    }
  }
}
