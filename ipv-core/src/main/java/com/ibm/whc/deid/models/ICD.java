/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.util.regex.Pattern;

public class ICD extends ICDWithoutFormat {

  public static final Pattern ICDV10_PATTERN = Pattern.compile("[A-Z][0-9]{2}\\.[0-9]{1,3}[A-Z]?");

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
