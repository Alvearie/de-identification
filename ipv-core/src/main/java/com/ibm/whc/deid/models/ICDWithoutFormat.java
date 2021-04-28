/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

public class ICDWithoutFormat {
  private final String code;
  private final String shortName;
  private final String fullName;
  private final String chapterCode;
  private final String chapterName;
  private final String categoryCode;
  private final String categoryName;

  /**
   * Instantiates a new ICD code.
   *
   * @param code the code
   * @param shortName the short name
   * @param fullName the full name
   * @param chapterCode the chapter code
   * @param chapterName the chapter name
   * @param categoryCode the category code
   * @param categoryName the category name
   */
  public ICDWithoutFormat(String code, String shortName, String fullName, String chapterCode, String chapterName,
      String categoryCode, String categoryName) {
    if (code == null || code.trim().isEmpty()) {
      throw new IllegalArgumentException("ICD code is missing");
    }
    if (shortName == null || shortName.trim().isEmpty()) {
      throw new IllegalArgumentException("ICD short name is missing");
    }
    if (fullName == null || fullName.trim().isEmpty()) {
      throw new IllegalArgumentException("ICD full bame is missing");
    }
    if (chapterCode == null || chapterCode.trim().isEmpty()) {
      throw new IllegalArgumentException("ICD chapter code is missing");
    }
    if (chapterName == null || chapterName.trim().isEmpty()) {
      throw new IllegalArgumentException("ICD chapter name is missing");
    }
    if (categoryCode == null || categoryCode.trim().isEmpty()) {
      throw new IllegalArgumentException("ICD category code is missing");
    }
    if (categoryName == null || categoryName.trim().isEmpty()) {
      throw new IllegalArgumentException("ICD category name is missing");
    }

    this.code = code;
    this.shortName = shortName;
    this.fullName = fullName;
    this.chapterCode = chapterCode;
    this.chapterName = chapterName;
    this.categoryCode = categoryCode;
    this.categoryName = categoryName;
  }

  /**
   * Gets category name.
   *
   * @return the category name
   */
  public String getCategoryName() {
    return categoryName;
  }

  /**
   * Gets category code.
   *
   * @return the category code
   */
  public String getCategoryCode() {
    return categoryCode;
  }

  /**
   * Gets chapter code.
   *
   * @return the chapter code
   */
  public String getChapterCode() {
    return chapterCode;
  }

  /**
   * Gets chapter name.
   *
   * @return the chapter name
   */
  public String getChapterName() {
    return chapterName;
  }

  /**
   * Gets code.
   *
   * @return the code
   */
  public String getCode() {
    return code;
  }

  /**
   * Gets short name.
   *
   * @return the short name
   */
  public String getShortName() {
    return shortName;
  }

  /**
   * Gets full name.
   *
   * @return the full name
   */
  public String getFullName() {
    return fullName;
  }

  @Override
  public String toString() {
    return this.code + ":" + this.fullName;
  }
}
