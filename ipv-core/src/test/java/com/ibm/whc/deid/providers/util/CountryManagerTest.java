/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.ibm.whc.deid.util.CountryManager;
import com.ibm.whc.deid.util.CountryNameSpecification;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class CountryManagerTest {
	private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;
  @Test
  public void testLookupSuccessful() throws Exception {
    CountryManager countryManager = new CountryManager(null, localizationProperty);
    String country = "United States of America";
    assertTrue(countryManager.isValidKey(country));
    assertTrue(countryManager.isValidCountry(country, CountryNameSpecification.NAME));

    // check that 3 ISO letter code is matched
    country = "USA";
    assertTrue(countryManager.isValidKey(country));
    assertTrue(countryManager.isValidCountry(country, CountryNameSpecification.ISO3));

    country = "GB";
    assertTrue(countryManager.isValidKey(country));
    assertTrue(countryManager.isValidCountry(country, CountryNameSpecification.ISO2));

    // check that the lowercase version is also matched
    country = "brazil";
    assertTrue(countryManager.isValidKey(country));

    // check that friendly name is also matched
    country = "Vietnam";
    assertTrue(countryManager.isValidKey(country));

    country = "Foobar";
    assertFalse(countryManager.isValidKey(country));
  }

  @Test
  public void testRandomCountryGenerator() throws Exception {
    CountryManager countryManager = new CountryManager(null, localizationProperty);
    // test random country
    assertTrue(
        countryManager.isValidKey(countryManager.getRandomKey(CountryNameSpecification.NAME)));

    String exceptionCountry = "US";
    for (int i = 0; i < 1000; i++) {
      String randomCountry = countryManager.getRandomKey();
      assertFalse(randomCountry.equals(exceptionCountry));
    }
  }

  @Test
  public void testClosestCountry() throws Exception {
    CountryManager countryManager = new CountryManager(null, localizationProperty);

    String originalCountry = "Greece";
    String[] neighbors = {"CYPRUS", "MALTA", "TURKEY", "SERBIA", "BOSNIA AND HERZEGOVINA",
        "ROMANIA", "MONTENEGRO", "BULGARIA", "ALBANIA", "GREECE", "ΕΛΛΆΔΑ", "HUNGARY"};

    List<String> neighborsList = Arrays.asList(neighbors);
    for (int i = 0; i < 100; i++) {
      String randomCountry = countryManager.getClosestCountry(originalCountry, 10);
      System.out.println(randomCountry);
      assertTrue(neighborsList.contains(randomCountry.toUpperCase()));
    }
  }

  @Test
  public void testClosestCountryOverrun() throws Exception {
    CountryManager countryManager = new CountryManager(null, localizationProperty);
    String originalCountry = "Greece";

    for (int i = 0; i < 100; i++) {
      String randomCountry = countryManager.getClosestCountry(originalCountry, 100000);
      assertTrue(countryManager.isValidKey(randomCountry));
    }
  }
}
