/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.util.ZIPCodeManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import org.junit.Test;

public class ZIPCodeManagerTest {
  String tenantId = "TEST_TENANT";

  @Test
  public void test() {
    ZIPCodeManager zipCodeManager = new ZIPCodeManager(3, tenantId);
    assertTrue(zipCodeManager.isValidKey("US", "00601"));
  }

  @Test
  public void testPopulation() {
    ZIPCodeManager zipCodeManager = new ZIPCodeManager(3, tenantId);

    Integer population = zipCodeManager.getPopulation("US", "00601");
    assertNotNull(population);
    assertEquals(18570, population.intValue());
  }

  @Test
  public void testPopulationThreeDigitPrefix() {

    ZIPCodeManager zipCodeManager = new ZIPCodeManager(3, tenantId);

    Integer population = zipCodeManager.getPopulationByPrefix("US", "00601");
    assertNotNull(population);
    assertEquals(1214568, population.intValue());
  }

  @Test
  public void testInvalidCountry() {
    ZIPCodeManager zipCodeManager = new ZIPCodeManager(3, tenantId);

    Integer population = zipCodeManager.getPopulationByPrefix("#$@", "00601");
    assertEquals(0, population.intValue());

    population = zipCodeManager.getPopulation("#$@", "00601");
    assertEquals(0, population.intValue());
  }

  @Test
  public void testInvalidCode() {
    ZIPCodeManager zipCodeManager = new ZIPCodeManager(3, tenantId);

    Integer population = zipCodeManager.getPopulationByPrefix("US", "!@$@%%");
    assertEquals(0, population.intValue());

    population = zipCodeManager.getPopulation("US", "!#!#!@#!#!");
    assertEquals(0, population.intValue());
  }

  /** Verify that the zip code length is properly acquired for a state that is known to have one. */
  @Test
  public void testZipCodeLength() {
    String zipLength =
        LocalizationManager.getInstance().getLocaleProperties("US").getProperty("zipcode.length");
    assertTrue(zipLength.equals("5"));
  }

  /** Verify that the zip code length of an invalid country is returned as null */
  @Test
  public void testZipCodeLengthInvalidCountry() {
    String zipLength =
        LocalizationManager.getInstance().getLocaleProperties("$#!").getProperty("zipcode.length");
    assertTrue(zipLength == null);
  }

  @Test
  public void testZipCodeReplacement() {
    System.out.println(LocalizationManager.getInstance().getLocaleProperties("US")
        .getProperty("zipcode.underPopulated"));
    assertTrue(LocalizationManager.getInstance().getLocaleProperties("US")
        .getProperty("zipcode.underPopulated").equals("000"));
  }

  @Test
  public void testZipCodeReplacementInvalidCountry() {
    assertTrue(LocalizationManager.getInstance().getLocaleProperties("$#!")
        .getProperty("zipcode.underPopulated") == null);
  }
}
