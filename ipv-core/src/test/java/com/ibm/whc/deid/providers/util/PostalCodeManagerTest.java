/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.List;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.models.PostalCode;
import com.ibm.whc.deid.util.PostalCodeManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;


public class PostalCodeManagerTest {
  private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  @Test
  public void testLookup() throws Exception {
    PostalCodeManager postalCodeManager =
        PostalCodeManager.buildPostalCodeManager(localizationProperty);
    String code = "99503";
    assertTrue(postalCodeManager.isValidKey(code));
    PostalCode postalCode = postalCodeManager.getValue(code);
    assertNotNull(postalCode);
    assertEquals(code, postalCode.getName());
    assertTrue(61.19 == postalCode.getLocation().getLatitude().doubleValue());
    assertTrue(-149.8938 == postalCode.getLocation().getLongitude().doubleValue());

    code = "xx";
    assertFalse(postalCodeManager.isValidKey(code));
    assertNull(postalCodeManager.getValue(code));

    code = null;
    assertFalse(postalCodeManager.isValidKey(code));
    assertNull(postalCodeManager.getValue(code));
  }

  @Test
  public void testRandomCodeGenerator() throws Exception {
    PostalCodeManager postalCodeManager =
        PostalCodeManager.buildPostalCodeManager(localizationProperty);
    assertTrue(postalCodeManager.isValidKey(postalCodeManager.getRandomKey()));
  }

  @Test
  @Ignore
  public void testPerformance() {
    PostalCodeManager postalCodeManager =
        PostalCodeManager.buildPostalCodeManager(localizationProperty);
    int N = 1000000;
    long startMillis = System.currentTimeMillis();
    String code = "99503";

    for (int i = 0; i < N; i++) {
      postalCodeManager.getClosestPostalCodes(code, 10);
    }

    long diff = System.currentTimeMillis() - startMillis;
    System.out.println(String.format("%d operations took %d milliseconds (%f per op)", N, diff,
        (double) diff / N));
    // Assert test always should finish in less than 10 seconds
    assertTrue(diff < 10000);
  }

  @Test
  public void testClosestCode() throws Exception {
    PostalCodeManager postalCodeManager =
        PostalCodeManager.buildPostalCodeManager(localizationProperty);
    String originalCode = "55901";

    List<PostalCode> nearest = postalCodeManager.getClosestPostalCodes(originalCode, 5);
    assertNotNull(nearest);
    assertEquals(5, nearest.size());
    assertEquals("55905", nearest.get(0).getName());
    assertEquals("55902", nearest.get(1).getName());
    assertEquals("55903", nearest.get(2).getName());
    assertEquals("55960", nearest.get(3).getName());
    assertEquals("55904", nearest.get(4).getName());

    nearest = postalCodeManager.getClosestPostalCodes("00000", 10);
    assertNotNull(nearest);
    assertEquals(0, nearest.size());
  }
}
