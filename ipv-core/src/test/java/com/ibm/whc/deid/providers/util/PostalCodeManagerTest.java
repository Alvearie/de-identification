/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertTrue;

import java.security.SecureRandom;
import java.util.Arrays;
import java.util.List;

import org.junit.Ignore;
import org.junit.Test;

import com.ibm.whc.deid.models.PostalCode;
import com.ibm.whc.deid.util.PostalCodeManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;


public class PostalCodeManagerTest {
  private String tenantId = "TEST_TENANT";
  private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  @Test
  public void testLookupSuccessful() throws Exception {
    PostalCodeManager postalCodeManager = new PostalCodeManager(null, localizationProperty);
    String code = "99503";

    assertTrue(postalCodeManager.isValidKey(code));
  }

  @Test
  public void testRandomCodeGenerator() throws Exception {
    PostalCodeManager postalCodeManager = new PostalCodeManager(null, localizationProperty);
    assertTrue(postalCodeManager.isValidKey(postalCodeManager.getRandomKey()));
  }

  @Test
  @Ignore
  public void testPerformance() {
    PostalCodeManager postalCodeManager = new PostalCodeManager(null, localizationProperty);
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
    PostalCodeManager postalCodeManager = new PostalCodeManager(tenantId, localizationProperty);
    String originalCode = "99529";
    String[] neighbors = {"99501", "99502", "99503", "99507", "99510", "99513", "99515", "99517",
        "99518", "99529", "99530", "99599"};

    List<String> neighborsList = Arrays.asList(neighbors);

    SecureRandom random = new SecureRandom();
    for (int i = 0; i < 100; i++) {
      List<PostalCode> nearest = postalCodeManager.getClosestPostalCodes(originalCode, 10);
      String returnedCode = "";
      if (nearest.size() > 0) {
        returnedCode = nearest.get(random.nextInt(nearest.size())).getName();
      } else {
        returnedCode = "NO_CODE_FOUND";
      }

      assertTrue(neighborsList.contains(returnedCode.toUpperCase()));
    }
  }
}
