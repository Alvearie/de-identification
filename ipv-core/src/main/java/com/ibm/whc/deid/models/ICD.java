/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

public class ICD extends ICDWithoutFormat {

  private final ICDFormat format;

  public ICD(ICDWithoutFormat icd, ICDFormat format) {
    super(icd.getCode(), icd.getShortName(), icd.getFullName(), icd.getChapterCode(),
        icd.getChapterName(), icd.getCategoryCode(), icd.getCategoryName());
    this.format = format;
  }

  public ICDFormat getFormat() {
    return format;
  }
}
