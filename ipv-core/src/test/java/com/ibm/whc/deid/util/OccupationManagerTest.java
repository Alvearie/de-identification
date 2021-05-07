/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.List;
import org.junit.Test;
import com.ibm.whc.deid.models.Occupation;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;

public class OccupationManagerTest implements MaskingProviderTest {

  @Test
  public void testMain() {
    OccupationManager occupationManager =
        OccupationManager.buildOccupationManager(localizationProperty);
    validateContent(occupationManager.getValues());
    validateContent(occupationManager.getValues("en"));
  }

  private void validateContent(List<Occupation> list) {
    assertEquals(20914, list.size());
    int total = 0;
    for (Occupation occ : list) {
      total += occ.getCategories().size();
    }
    assertEquals(25603, total);
  }

  @Test
  public void testLookup() {
    OccupationManager occupationManager =
        OccupationManager.buildOccupationManager(localizationProperty);

    String occupation = "actor";
    assertTrue(occupationManager.isValidKey(occupation));
    assertTrue(occupationManager.isValidKey(occupation.toLowerCase()));
    assertTrue(occupationManager.isValidKey(occupation.toUpperCase()));
    occupation = "acTor";
    assertTrue(occupationManager.isValidKey(occupation));
    
    occupation = "foreman";
    Occupation occ = occupationManager.getValue(occupation);
    assertNotNull(occ);
    assertEquals(52, occ.getCategories().size());

    occupation = "Accountant, district";
    occ = occupationManager.getValue(occupation);
    assertNotNull(occ);
    assertEquals(1, occ.getCategories().size());
    assertEquals("Chartered and certified accountants", occ.getCategories().get(0));
  }

  @Test
  public void testFalsePositives() {
    String[] values = {"C", "Z", "S", "P", "N", "G", "O", "-"};
    OccupationManager occupationManager =
        OccupationManager.buildOccupationManager(localizationProperty);
    for (String value : values) {
      assertFalse(occupationManager.isValidKey(value));
    }
  }

  @Test
  public void testBadInput() throws Exception {
    try {
      OccupationManager.buildOccupationManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=2, values=[Accountant, forensic, ]] from /localization/test.occupation.bad.csv: The value \"\" for \"occupation category\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    String locale = "us";
    OccupationManager manager = new OccupationManager();

    String[] record = new String[] {"programmer", "painful occupations"};
    OccupationManager.loadRecord(locale, manager, record);
    Occupation resource = manager.getValue("ProgrammER");
    assertNotNull(resource);
    assertEquals("programmer", resource.getName());
    assertEquals(locale, resource.getNameCountryCode());
    List<String> categories = resource.getCategories();
    assertNotNull(categories);
    assertEquals(1, categories.size());
    assertEquals("painful occupations", categories.get(0));
    assertSame(resource, manager.getValue(locale, "PROGRAMMER"));

    record[1] = "computer jobs";
    OccupationManager.loadRecord(locale, manager, record);
    assertEquals(1, manager.getValues().size());
    assertEquals(1, manager.getValues(locale).size());
    Occupation resource2 = manager.getValue("ProgrammER");
    assertSame(resource, resource2);
    assertEquals("programmer", resource2.getName());
    assertEquals(locale, resource2.getNameCountryCode());
    categories = resource2.getCategories();
    assertNotNull(categories);
    assertEquals(2, categories.size());
    assertEquals("painful occupations", categories.get(0));
    assertEquals("computer jobs", categories.get(1));
    assertSame(resource2, manager.getValue(locale, "PROGRAMMER"));

    record[0] = "firmware developer";

    // bad name
    String temp = record[0];
    record[0] = null;
    try {
      OccupationManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("occupation name"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[0] = " ";
    try {
      OccupationManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("occupation name"));
    }
    record[0] = temp;

    // bad category
    temp = record[1];
    record[1] = null;
    try {
      OccupationManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("occupation category"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[1] = "  ";
    try {
      OccupationManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("occupation category"));
    }
    record[1] = temp;

    // bad locale
    temp = locale;
    locale = null;
    try {
      OccupationManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("occupation locale"));
      assertTrue(e.getMessage().contains("null"));
    }
    locale = " ";
    try {
      OccupationManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("occupation locale"));
    }
    locale = temp;

    // bad add
    Occupation occ = new Occupation("name1", locale, "cat1");
    try {
      occ.addCategory(null);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("occupation category"));
      assertTrue(e.getMessage().contains("null"));
    }
    try {
      occ.addCategory("  ");
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("occupation category"));
    }
  }
}
