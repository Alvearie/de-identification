/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.models.ICD;
import com.ibm.whc.deid.models.ICDFormat;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;

public class ICDv9ManagerTest implements MaskingProviderTest {

  @Test
  public void testBadInput() throws Exception {
    try {
      ICDv9Manager.buildICDv9Manager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=3, values=[TESTX00.0, TestX, Test Name X,             , Chapter Name X, X10-X19, Category Name X]] from /localization/test.icdv9.bad.csv: The value \"            \" for \"ICD chapter code\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    String[] record =
        new String[] {"code", "shortName", "fullName", "chapterCode", "chapterName", "categoryCode",
            "categoryName"};
    ICDv9Manager manager = new ICDv9Manager();
    ICDv9Manager.loadRecord(manager, record);
    ICD icd = manager.lookupICD("code");
    assertNotNull(icd);
    assertEquals("code", icd.getCode());
    assertEquals("fullName", icd.getFullName());
    assertEquals("categoryCode", icd.getCategoryCode());
    assertEquals("categoryName", icd.getCategoryName());
    assertEquals("chapterCode", icd.getChapterCode());
    assertEquals("chapterName", icd.getChapterName());
    assertEquals(ICDFormat.CODE, icd.getFormat());
    icd = manager.lookupICD("FULLNAME");
    assertNotNull(icd);
    assertEquals("code", icd.getCode());
    assertEquals("fullName", icd.getFullName());
    assertEquals("categoryCode", icd.getCategoryCode());
    assertEquals("categoryName", icd.getCategoryName());
    assertEquals("chapterCode", icd.getChapterCode());
    assertEquals("chapterName", icd.getChapterName());
    assertEquals(ICDFormat.NAME, icd.getFormat());
    icd = manager.lookupICD("SHORTname");
    assertNotNull(icd);
    assertEquals("code", icd.getCode());
    assertEquals("fullName", icd.getFullName());
    assertEquals("categoryCode", icd.getCategoryCode());
    assertEquals("categoryName", icd.getCategoryName());
    assertEquals("chapterCode", icd.getChapterCode());
    assertEquals("chapterName", icd.getChapterName());
    assertEquals(ICDFormat.NAME, icd.getFormat());

    // bad input
    String temp = record[0];
    record[0] = null;
    try {
      ICDv9Manager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ICD code"));
    }
    record[0] = " ";
    try {
      ICDv9Manager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ICD code"));
    }
    record[0] = temp;

    temp = record[1];
    record[1] = null;
    try {
      ICDv9Manager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ICD short name"));
    }
    record[1] = "  ";
    try {
      ICDv9Manager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ICD short name"));
    }
    record[1] = temp;

    temp = record[2];
    record[2] = null;
    try {
      ICDv9Manager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ICD full name"));
    }
    record[2] = "   ";
    try {
      ICDv9Manager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ICD full name"));
    }
    record[2] = temp;

    temp = record[3];
    record[3] = null;
    try {
      ICDv9Manager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ICD chapter code"));
    }
    record[3] = "   ";
    try {
      ICDv9Manager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ICD chapter code"));
    }
    record[3] = temp;

    temp = record[4];
    record[4] = "  ";
    try {
      ICDv9Manager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ICD chapter name"));
    }
    record[4] = null;
    try {
      ICDv9Manager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ICD chapter name"));
    }
    record[4] = temp;

    temp = record[5];
    record[5] = "  ";
    try {
      ICDv9Manager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ICD category code"));
    }
    record[5] = null;
    try {
      ICDv9Manager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ICD category code"));
    }
    record[5] = temp;

    temp = record[6];
    record[6] = "  ";
    try {
      ICDv9Manager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ICD category name"));
    }
    record[6] = null;
    try {
      ICDv9Manager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ICD category name"));
    }
    record[6] = temp;
  }
}
