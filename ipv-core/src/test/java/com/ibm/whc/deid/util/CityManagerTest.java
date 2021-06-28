/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.models.City;
import com.ibm.whc.deid.models.LatitudeLongitude;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;

public class CityManagerTest implements MaskingProviderTest {

  @Test
  public void testBadInput() throws Exception {
    try {
      CityManager.buildCityManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=6, values=[, 10.0, 11.0, IT, 000000]] from /localization/test.city.bad.csv: The value \"\" for \"city name\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    String[] record =
        new String[] {"Rochester", "23.3", "-78.3", "Canada"};
    CityManager manager = new CityManager();
    String locale = "EN";

    CityManager.loadRecord(locale, manager, record);
    City city = manager.getValue("rochester");
    assertEquals("Rochester", city.getName());
    LatitudeLongitude location = city.getLocation();
    assertNotNull(location);
    assertEquals(23.3, location.getLatitude(), 0);
    assertEquals(-78.3, location.getLongitude(), 0);
    assertEquals("EN", city.getNameCountryCode());
    assertEquals(city, manager.getValue("eN", "ROCHESTER"));

    record = new String[] {"Byron", null, "-78.3", "Canada"};
    CityManager.loadRecord(locale, manager, record);
    city = manager.getValue("byron");
    assertEquals("Byron", city.getName());
    assertNull(city.getLocation());
    assertEquals("EN", city.getNameCountryCode());
    assertEquals(city, manager.getValue("eN", "BYRON"));

    record = new String[] {"Kasson", "", "-78.3", "Canada"};
    CityManager.loadRecord(locale, manager, record);
    city = manager.getValue("kasson");
    assertEquals("Kasson", city.getName());
    assertNull(city.getLocation());
    assertEquals("EN", city.getNameCountryCode());
    assertEquals(city, manager.getValue("eN", "KASSON"));

    record = new String[] {"Pine Island", "54.19", null, "Canada"};
    CityManager.loadRecord(locale, manager, record);
    city = manager.getValue("pine island");
    assertEquals("Pine Island", city.getName());
    assertNull(city.getLocation());
    assertEquals("EN", city.getNameCountryCode());
    assertEquals(city, manager.getValue("eN", "PINE ISLAND"));

    record = new String[] {"Zumbrota", "54.19", " ", "Canada"};
    CityManager.loadRecord(locale, manager, record);
    city = manager.getValue("zumbrota");
    assertEquals("Zumbrota", city.getName());
    assertNull(city.getLocation());
    assertEquals("EN", city.getNameCountryCode());
    assertEquals(city, manager.getValue("eN", "ZUMBROTA"));

    record = new String[] {"Rochester", "23.3", "-78.3"};

    // bad name
    String temp = record[0];
    record[0] = null;
    try {
      CityManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("city name"));
    }
    record[0] = " ";
    try {
      CityManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("city name"));
    }
    record[0] = temp;

    // bad latitude
    temp = record[1];
    record[1] = "AX";
    try {
      CityManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("latitude"));
    }
    record[1] = "-91.4";
    try {
      CityManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("latitude"));
    }
    record[1] = "91.4";
    try {
      CityManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("latitude"));
    }
    record[1] = temp;

    // bad longitude
    temp = record[2];
    record[2] = "AX";
    try {
      CityManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("longitude"));
    }
    record[2] = "-181.4";
    try {
      CityManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("longitude"));
    }
    record[2] = "191.4";
    try {
      CityManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("longitude"));
    }
    record[2] = temp;

    // bad locale
    temp = locale;
    locale = null;
    try {
      CityManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("city locale"));
    }
    locale = " ";
    try {
      CityManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("city locale"));
    }
    record[0] = temp;
    locale = temp;
  }
}
