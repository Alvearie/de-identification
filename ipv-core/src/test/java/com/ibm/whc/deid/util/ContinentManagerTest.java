/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.models.Continent;
import com.ibm.whc.deid.models.LatitudeLongitude;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;

public class ContinentManagerTest implements MaskingProviderTest {

  @Test
  public void testGetClosest() throws Exception {
    ContinentManager manager = ContinentManager.buildContinentManager(localizationProperty);
    
    Continent source = manager.getValue("europe");
    assertNotNull(source);
    
    boolean foundAfrica = false;
    boolean foundAsia = false;
    for (int i = 0; i < 20; i++) {
      Continent selected = manager.getClosestContinent(source, 2);
      assertNotNull(selected);
      String name = selected.getName();
      assertTrue(name.equals("Africa") || name.equals("Asia"));
      if (name.equals("Africa")) {
        foundAfrica = true;
      }
      if (name.equals("Asia")) {
        foundAsia = true;
      }
    }
    assertTrue(foundAfrica);
    assertTrue(foundAsia);
  }

  @Test
  public void testGetClosest_badK() throws Exception {
    ContinentManager manager = ContinentManager.buildContinentManager(localizationProperty);
    Continent source = manager.getValue("europe");
    assertNotNull(source);
    assertNull(manager.getClosestContinent(source, 0));
    assertNull(manager.getClosestContinent(source, -1));
  }

  @Test
  public void testBadInput() throws Exception {
    try {
      ContinentManager.buildContinentManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=6, values=[South America, -114.4793678, -57.8555657]] from /localization/test.continent.bad.csv: The value \"-114.4793678\" for \"latitude\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    ContinentManager manager = new ContinentManager();
    String locale = "en";

    String[] record = new String[] {"C1", null, null};
    ContinentManager.loadRecord(locale, manager, record);
    Continent continent = manager.getValue("c1");
    assertNotNull(continent);
    assertEquals("C1", continent.getName());
    assertEquals(locale, continent.getNameCountryCode());
    assertNull(continent.getLocation());
    assertSame(continent, manager.getValue(locale, "c1"));

    record = new String[] {"c2", " ", "100.0"};
    ContinentManager.loadRecord(locale, manager, record);
    continent = manager.getValue("C2");
    assertNotNull(continent);
    assertEquals("c2", continent.getName());
    assertEquals(locale, continent.getNameCountryCode());
    assertNull(continent.getLocation());
    assertSame(continent, manager.getValue(locale, "c2"));

    record = new String[] {"c3", "0.1", " "};
    ContinentManager.loadRecord(locale, manager, record);
    continent = manager.getValue("C3");
    assertNotNull(continent);
    assertEquals("c3", continent.getName());
    assertEquals(locale, continent.getNameCountryCode());
    assertNull(continent.getLocation());
    assertSame(continent, manager.getValue(locale, "c3"));

    record = new String[] {"Asia", "23.3", "-78.3"};

    ContinentManager.loadRecord(locale, manager, record);
    continent = manager.getValue("asia");
    assertNotNull(continent);
    assertEquals("Asia", continent.getName());
    assertEquals(locale, continent.getNameCountryCode());
    LatitudeLongitude location = continent.getLocation();
    assertNotNull(location);
    assertEquals(23.3, location.getLatitude(), 0);
    assertEquals(-78.3, location.getLongitude(), 0);
    assertSame(continent, manager.getValue(locale, "asIA"));

    // bad name
    String temp = record[0];
    record[0] = null;
    try {
      ContinentManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("continent name"));
    }
    record[0] = " ";
    try {
      ContinentManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("continent name"));
    }
    record[0] = temp;

    // bad latitude
    temp = record[1];
    record[1] = "AX";
    try {
      ContinentManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("latitude"));
    }
    record[1] = "-91.4";
    try {
      ContinentManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("latitude"));
    }
    record[1] = "91.4";
    try {
      ContinentManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("latitude"));
    }
    record[1] = temp;

    // bad longitude
    temp = record[2];
    record[2] = "AX";
    try {
      ContinentManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("longitude"));
    }
    record[2] = "-181.4";
    try {
      ContinentManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("longitude"));
    }
    record[2] = "191.4";
    try {
      ContinentManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("longitude"));
    }
    record[2] = temp;

    // bad locale
    temp = locale;
    locale = null;
    try {
      ContinentManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("continent locale"));
    }
    locale = " ";
    try {
      ContinentManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("continent locale"));
    }
    locale = temp;
  }
}
