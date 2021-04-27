/*
 * (C) Copyright IBM Corp. 2016,2020
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
import com.ibm.whc.deid.providers.masking.TestLogSetUp;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class CountryManagerTest extends TestLogSetUp {
  private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  @Test
  public void testBadInput() throws Exception {
    try {
      CountryManager
          .buildCountryManager("/localization/test.country.bad_long.localization.properties");
      fail("expected exception");
    } catch (RuntimeException e) {
      assertEquals("longitude is out of range: 819.8", e.getMessage());
      String logs = outContent.toString();
      assertTrue(logs, logs.contains(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=3, values=[Albania, AL, ALB, , Europe, 41.333, 819.800]] from file /localization/test.country.bad_long.csv: longitude is out of range: 819.8"));
    }

    try {
      CountryManager
          .buildCountryManager("/localization/test.country.bad_name.localization.properties");
      fail("expected exception");
    } catch (RuntimeException e) {
      assertEquals("`name` is missing", e.getMessage());
    }
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
            "ROMANIA", "MONTENEGRO", "BULGARIA", "ALBANIA", "GREECE", "HUNGARY"));
    for (int i = 0; i < 30; i++) {
      String randomCountry = countryManager.getClosestCountry(originalCountry, 10);
      assertNotNull(randomCountry);
      // System.out.println(randomCountry);
      assertTrue(neighbors.contains(randomCountry.toUpperCase()));
      Country country = countryManager.getValue(randomCountry);
      assertNotNull(country);
      assertEquals(CountryNameSpecification.NAME, country.getCountryNameSpecification());
    }

    originalCountry = "Gr";
    neighbors = new HashSet<>(
        Arrays.asList("CY", "MT", "TR", "RS", "BA", "RO", "ME", "BG", "AL", "GR", "HU"));
    for (int i = 0; i < 30; i++) {
      String randomCountry = countryManager.getClosestCountry(originalCountry, 10);
      // System.out.println(randomCountry);
      assertTrue(neighbors.contains(randomCountry.toUpperCase()));
      Country country = countryManager.getValue(randomCountry);
      assertNotNull(country);
      assertEquals(CountryNameSpecification.ISO2, country.getCountryNameSpecification());
    }

    originalCountry = "GrC";
    neighbors = new HashSet<>(
        Arrays.asList("CYP", "MLT", "TUR", "SRB", "BIH", "ROU", "MNE", "BGR", "ALB", "GRC", "HUN"));
    for (int i = 0; i < 30; i++) {
      String randomCountry = countryManager.getClosestCountry(originalCountry, 10);
      // System.out.println(randomCountry);
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
