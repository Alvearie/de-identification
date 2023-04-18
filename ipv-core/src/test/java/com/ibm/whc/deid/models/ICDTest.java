/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class ICDTest {

  @Test
  public void testMain() {
    ICDWithoutFormat icdw = new ICDWithoutFormat("code", "short", "full", "chapterCode",
        "chapterName", "catCode", "catName");
    assertEquals("code", icdw.getCode());
    assertEquals("short", icdw.getShortName());
    assertEquals("full", icdw.getFullName());
    assertEquals("chapterCode", icdw.getChapterCode());
    assertEquals("chapterName", icdw.getChapterName());
    assertEquals("catCode", icdw.getCategoryCode());
    assertEquals("catName", icdw.getCategoryName());
    assertEquals("code:full", icdw.toString());

    ICD icd = new ICD(icdw, ICDFormat.CODE);
    assertEquals("code", icd.getCode());
    assertEquals("short", icd.getShortName());
    assertEquals("full", icd.getFullName());
    assertEquals("chapterCode", icd.getChapterCode());
    assertEquals("chapterName", icd.getChapterName());
    assertEquals("catCode", icd.getCategoryCode());
    assertEquals("catName", icd.getCategoryName());
    assertEquals("code:full", icd.toString());
    assertEquals(ICDFormat.CODE, icd.getFormat());
  }
}
