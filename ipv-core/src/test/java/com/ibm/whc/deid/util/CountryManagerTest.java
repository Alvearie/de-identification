/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.Arrays;
import java.util.HashSet;
import org.junit.Test;
import com.ibm.whc.deid.models.Country;
import com.ibm.whc.deid.models.LatitudeLongitude;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;

public class CountryManagerTest implements MaskingProviderTest {

  @Test
  public void testBadInput() throws Exception {
    try {
      CountryManager
          .buildCountryManager("/localization/test.country.bad_long.localization.properties");
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=3, values=[Albania, AL, ALB, , Europe, 41.333, 819.800]] from /localization/test.country.bad_long.csv: The value \"819.8\" for \"longitude\" is invalid",
          e.getMessage());
    }

    try {
      CountryManager
          .buildCountryManager("/localization/test.country.bad_name.localization.properties");
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=3, values=[ , AL, ALB, , Europe, 41.333, 19.800]] from /localization/test.country.bad_name.csv: The value \" \" for \"country name\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    CountryManager manager = new CountryManager();
    String locale = "en";

    String[] record = new String[] {"France", null, "  ", "", "Unknown", null, ""};
    CountryManager.loadRecord(locale, manager, record);
    Country country = manager.getValue("france");
    assertNotNull(country);
    assertEquals("France", country.getName());
    assertNull(country.getName(CountryNameSpecification.ISO2));
    assertNull(country.getName(CountryNameSpecification.ISO3));
    assertNull(country.getContinent());
    assertNull(country.getLocation());
    assertEquals(CountryNameSpecification.NAME, country.getCountryNameSpecification());

    record = new String[] {"Germany", " ", null, null, " ", "", null};
    CountryManager.loadRecord(locale, manager, record);
    country = manager.getValue("germany");
    assertNotNull(country);
    assertEquals("Germany", country.getName());
    assertNull(country.getName(CountryNameSpecification.ISO2));
    assertNull(country.getName(CountryNameSpecification.ISO3));
    assertNull(country.getContinent());
    assertNull(country.getLocation());
    assertEquals(CountryNameSpecification.NAME, country.getCountryNameSpecification());

    record = new String[] {"Canada", "CA", "CAN", "home", "north america", "23.3", "-78.3"};
    CountryManager.loadRecord(locale, manager, record);
    country = manager.getValue("canada");
    assertNotNull(country);
    assertEquals("Canada", country.getName());
    assertEquals("CA", country.getName(CountryNameSpecification.ISO2));
    assertEquals("CAN", country.getName(CountryNameSpecification.ISO3));
    assertEquals("north america", country.getContinent());
    LatitudeLongitude location = country.getLocation();
    assertNotNull(location);
    assertEquals(23.3, location.getLatitude(), 0);
    assertEquals(-78.3, location.getLongitude(), 0);
    assertEquals(CountryNameSpecification.NAME, country.getCountryNameSpecification());

    country = manager.getValue("ca");
    assertNotNull(country);
    assertEquals("Canada", country.getName());
    assertEquals("CA", country.getName(CountryNameSpecification.ISO2));
    assertEquals("CAN", country.getName(CountryNameSpecification.ISO3));
    assertEquals("north america", country.getContinent());
    location = country.getLocation();
    assertNotNull(location);
    assertEquals(23.3, location.getLatitude(), 0);
    assertEquals(-78.3, location.getLongitude(), 0);
    assertEquals(CountryNameSpecification.ISO2, country.getCountryNameSpecification());

    country = manager.getValue("can");
    assertNotNull(country);
    assertEquals("Canada", country.getName());
    assertEquals("CA", country.getName(CountryNameSpecification.ISO2));
    assertEquals("CAN", country.getName(CountryNameSpecification.ISO3));
    assertEquals("north america", country.getContinent());
    location = country.getLocation();
    assertNotNull(location);
    assertEquals(23.3, location.getLatitude(), 0);
    assertEquals(-78.3, location.getLongitude(), 0);
    assertEquals(CountryNameSpecification.ISO3, country.getCountryNameSpecification());

    country = manager.getValue("HOME");
    assertNotNull(country);
    assertEquals("Canada", country.getName());
    assertEquals("CA", country.getName(CountryNameSpecification.ISO2));
    assertEquals("CAN", country.getName(CountryNameSpecification.ISO3));
    assertEquals("north america", country.getContinent());
    location = country.getLocation();
    assertNotNull(location);
    assertEquals(23.3, location.getLatitude(), 0);
    assertEquals(-78.3, location.getLongitude(), 0);
    assertEquals(CountryNameSpecification.NAME, country.getCountryNameSpecification());

    // bad name
    String temp = record[0];
    record[0] = null;
    try {
      CountryManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("country name"));
    }
    record[0] = " ";
    try {
      CountryManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("country name"));
    }
    record[0] = temp;

    // bad ISO2
    temp = record[1];
    record[1] = "AXE";
    try {
      CountryManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("country iso2code"));
    }
    record[1] = " X";
    try {
      CountryManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("country iso2code"));
    }
    record[1] = temp;

    // bad ISO3
    temp = record[2];
    record[2] = "AX";
    try {
      CountryManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("country iso3code"));
    }
    record[2] = "AX ";
    try {
      CountryManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("country iso3code"));
    }
    record[2] = "POOL";
    try {
      CountryManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("country iso3code"));
    }
    record[2] = temp;

    // bad latitude
    temp = record[5];
    record[5] = "AX";
    try {
      CountryManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("latitude"));
    }
    record[5] = "-91.4";
    try {
      CountryManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("latitude"));
    }
    record[5] = "91.4";
    try {
      CountryManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("latitude"));
    }
    record[5] = temp;

    // bad longitude
    temp = record[6];
    record[6] = "AX";
    try {
      CountryManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("longitude"));
    }
    record[6] = "-181.4";
    try {
      CountryManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("longitude"));
    }
    record[6] = "191.4";
    try {
      CountryManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("longitude"));
    }
    record[6] = temp;

    // bad locale
    temp = locale;
    locale = null;
    try {
      CountryManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("country locale"));
      assertTrue(e.getMessage().contains("null"));
    }
    locale = " ";
    try {
      CountryManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("country locale"));
    }
    locale = temp;

    // friendly name not loaded when null or missing
    int count = manager.countryNames.getKeys().size();
    String[] record3 =
        new String[] {"country3", "c3", "cc3", null, "north america", "23.3", "-78.3"};
    CountryManager.loadRecord(locale, manager, record3);
    assertNotNull(manager.getValue("c3"));
    assertEquals(count + 1, manager.countryNames.getKeys().size());
    String[] record4 =
        new String[] {"country4", "c4", "cc4", "  ", "north america", "23.3", "-78.3"};
    CountryManager.loadRecord(locale, manager, record4);
    assertNotNull(manager.getValue("cc4"));
    assertEquals(count + 2, manager.countryNames.getKeys().size());
  }

  @Test
  public void testGetValue() throws Exception {
    CountryManager countryManager = CountryManager.buildCountryManager(localizationProperty);

    // check formal name
    String original = "UniTED States of America";
    Country country = countryManager.getValue(original);
    assertNotNull(country);
    assertEquals(CountryNameSpecification.NAME, country.getCountryNameSpecification());
    assertEquals("United States of America", country.getName());
    assertTrue(countryManager.isValidKey(original));

    // check that 3 ISO letter code is matched
    original = "UsA";
    country = countryManager.getValue(original);
    assertNotNull(country);
    assertEquals(CountryNameSpecification.ISO3, country.getCountryNameSpecification());
    assertEquals("United States of America", country.getName());
    assertTrue(countryManager.isValidKey(original));

    // check that 2 ISO letter code is matched
    original = "Us";
    country = countryManager.getValue(original);
    assertNotNull(country);
    assertEquals(CountryNameSpecification.ISO2, country.getCountryNameSpecification());
    assertEquals("United States of America", country.getName());
    assertTrue(countryManager.isValidKey(original));

    // check that friendly name is also matched
    original = "United STATES";
    country = countryManager.getValue(original);
    assertNotNull(country);
    assertEquals(CountryNameSpecification.NAME, country.getCountryNameSpecification());
    assertEquals("United States of America", country.getName());
    assertTrue(countryManager.isValidKey(original));

    original = "Foobar";
    assertNull(countryManager.getValue(original));
    assertFalse(countryManager.isValidKey(original));
  }

  @Test
  public void testRandomValue() throws Exception {
    CountryManager countryManager = CountryManager.buildCountryManager(localizationProperty);

    for (int i = 0; i < 20; i++) {
      Country random = countryManager.getRandomValue();
      assertNotNull(random);
      assertEquals(CountryNameSpecification.NAME, random.getCountryNameSpecification());
    }

    for (int i = 0; i < 20; i++) {
      Country random = countryManager.getRandomValue(null);
      assertNotNull(random);
      assertEquals(CountryNameSpecification.NAME, random.getCountryNameSpecification());
    }

    for (int i = 0; i < 20; i++) {
      Country random = countryManager.getRandomValue(CountryNameSpecification.ISO2);
      assertNotNull(random);
      assertEquals(CountryNameSpecification.ISO2, random.getCountryNameSpecification());
    }

    for (int i = 0; i < 20; i++) {
      Country random = countryManager.getRandomValue(CountryNameSpecification.ISO3);
      assertNotNull(random);
      assertEquals(CountryNameSpecification.ISO3, random.getCountryNameSpecification());
    }

    for (int i = 0; i < 20; i++) {
      Country random = countryManager.getRandomValue(CountryNameSpecification.NAME);
      assertNotNull(random);
      assertEquals(CountryNameSpecification.NAME, random.getCountryNameSpecification());
    }
  }

  @Test
  public void testClosestCountry() throws Exception {
    CountryManager countryManager = CountryManager.buildCountryManager(localizationProperty);

    String originalCountry = "Greece";
    HashSet<String> neighbors =
        new HashSet<>(Arrays.asList("CYPRUS", "MALTA", "TURKEY", "SERBIA", "BOSNIA AND HERZEGOVINA",
            "ROMANIA", "MONTENEGRO", "BULGARIA", "ALBANIA", "GREECE", "HUNGARY",
            "MACEDONIA (THE FORMER YUGOSLAV REPUBLIC OF)"));
    for (int i = 0; i < 40; i++) {
      String randomCountry = countryManager.getClosestCountry(originalCountry, 10);
      assertNotNull(randomCountry);
      assertTrue(randomCountry, neighbors.contains(randomCountry.toUpperCase()));
      Country country = countryManager.getValue(randomCountry);
      assertNotNull(country);
      assertEquals(CountryNameSpecification.NAME, country.getCountryNameSpecification());
    }

    originalCountry = "Gr";
    neighbors = new HashSet<>(
        Arrays.asList("CY", "MT", "TR", "RS", "BA", "RO", "ME", "BG", "AL", "GR", "HU", "MK"));
    for (int i = 0; i < 40; i++) {
      String randomCountry = countryManager.getClosestCountry(originalCountry, 10);
      assertTrue(neighbors.contains(randomCountry.toUpperCase()));
      Country country = countryManager.getValue(randomCountry);
      assertNotNull(country);
      assertEquals(CountryNameSpecification.ISO2, country.getCountryNameSpecification());
    }

    originalCountry = "GrC";
    neighbors = new HashSet<>(
        Arrays.asList("CYP", "MLT", "TUR", "SRB", "BIH", "ROU", "MNE", "BGR", "ALB", "GRC", "HUN",
            "MKD"));
    for (int i = 0; i < 40; i++) {
      String randomCountry = countryManager.getClosestCountry(originalCountry, 10);
      assertTrue(neighbors.contains(randomCountry.toUpperCase()));
      Country country = countryManager.getValue(randomCountry);
      assertNotNull(country);
      assertEquals(CountryNameSpecification.ISO3, country.getCountryNameSpecification());
    }

    assertNull(countryManager.getClosestCountry("XX", 10));
  }

  @Test
  public void testClosestCountryOverrun() throws Exception {
    CountryManager countryManager = CountryManager.buildCountryManager(localizationProperty);

    String originalCountry = "Greece";

    for (int i = 0; i < 40; i++) {
      String randomCountry = countryManager.getClosestCountry(originalCountry, 100000);
      assertNotNull(randomCountry);
      Country country = countryManager.getValue(randomCountry);
      assertNotNull(country);
      assertEquals(CountryNameSpecification.NAME, country.getCountryNameSpecification());
    }
  }

  @Test
  public void testGetPseudorandom() throws Exception {
    CountryManager countryManager = CountryManager.buildCountryManager(localizationProperty);

    String original = "Greece";
    String ps = countryManager.getPseudorandom(original);
    assertNotNull(ps);
    Country country = countryManager.getValue(ps);
    assertNotNull(country);
    assertEquals(CountryNameSpecification.NAME, country.getCountryNameSpecification());
    for (int i = 0; i < 10; i++) {
      assertEquals(ps, countryManager.getPseudorandom(original));
    }

    original = "Gr";
    ps = countryManager.getPseudorandom(original);
    assertNotNull(ps);
    assertEquals(2, ps.length());
    country = countryManager.getValue(ps);
    assertNotNull(country);
    assertEquals(CountryNameSpecification.ISO2, country.getCountryNameSpecification());
    for (int i = 0; i < 10; i++) {
      assertEquals(ps, countryManager.getPseudorandom(original));
    }

    original = "Grc";
    ps = countryManager.getPseudorandom(original);
    assertNotNull(ps);
    assertEquals(3, ps.length());
    country = countryManager.getValue(ps);
    assertNotNull(country);
    assertEquals(CountryNameSpecification.ISO3, country.getCountryNameSpecification());
    for (int i = 0; i < 10; i++) {
      assertEquals(ps, countryManager.getPseudorandom(original));
    }

    original = "xxxx";
    ps = countryManager.getPseudorandom(original);
    assertNotNull(ps);
    country = countryManager.getValue(ps);
    assertNotNull(country);
    assertEquals(CountryNameSpecification.NAME, country.getCountryNameSpecification());
    for (int i = 0; i < 10; i++) {
      assertEquals(ps, countryManager.getPseudorandom(original));
    }
  }

  @Test
  public void testRandomKey() throws Exception {
    CountryManager countryManager = CountryManager.buildCountryManager(localizationProperty);

    for (int i = 0; i < 100; i++) {
      String key = countryManager.getRandomKey();
      assertNotNull(key);
      Country country = countryManager.getValue(key);
      assertNotNull(country);
      assertEquals(CountryNameSpecification.NAME, country.getCountryNameSpecification());
    }
  }
}
