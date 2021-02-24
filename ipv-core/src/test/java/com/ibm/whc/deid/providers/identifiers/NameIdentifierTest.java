/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class NameIdentifierTest implements MaskingProviderTest {
  @Test
  public void testIsOfThisType() throws Exception {
		NameIdentifier identifier = new NameIdentifier(tenantId, localizationProperty);

    assertFalse(identifier.isOfThisType("12308u499234802"));
    assertFalse(identifier.isOfThisType("84032-43092-3242"));

    assertFalse(identifier.isOfThisType("32 Carter Avn."));
    assertFalse(identifier.isOfThisType("15 Kennedy Avenue"));
    assertFalse(identifier.isOfThisType("Thompson Avn 1000"));

    assertTrue(identifier.isOfThisType("Carroll, John J."));
    assertTrue(identifier.isOfThisType("Carroll, John"));
    assertTrue(identifier.isOfThisType("Patrick K. Fitzgerald"));
    assertTrue(identifier.isOfThisType("Patrick Fitzgerald"));
    assertTrue(identifier.isOfThisType("Kennedy John"));
  }

  public List<String> fileContentsAsList(InputStream inputStream) throws Exception {
    List<String> lines = new ArrayList<>();

    BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));

    String line = reader.readLine();
    while (line != null) {
      lines.add(line);
      line = reader.readLine();
    }
    reader.close();

    return lines;
  }

  @Test
  public void testSurnamesTop1000US() throws Exception {
		NameIdentifier identifier = new NameIdentifier(tenantId, localizationProperty);
    String filename = "/top1KsurnamesUS.csv";
    List<String> surnames = fileContentsAsList(this.getClass().getResourceAsStream(filename));

    int totalSurnames = 0;
    int totalMatches = 0;

    for (String surname : surnames) {
      if (identifier.isOfThisType(surname)) {
        totalMatches += 1;
      }
    }

    assertTrue(((double) totalMatches / (double) totalSurnames) > 0.95);
  }

  @Test
  public void testNamesTop1000US() throws Exception {
		NameIdentifier identifier = new NameIdentifier(tenantId, localizationProperty);
    String[] filenames = {"/top1200maleNamesUS.csv", "/top1000femaleNamesUS.csv"};

    for (String filename : filenames) {
      List<String> names = fileContentsAsList(this.getClass().getResourceAsStream(filename));

      int totalNames = 0;
      int totalMatches = 0;

      for (String name : names) {
        totalNames += 1;
        if (identifier.isOfThisType(name)) {
          totalMatches += 1;
        }
      }

      assertTrue(((double) totalMatches / (double) totalNames) > 0.70);
    }
  }
}
