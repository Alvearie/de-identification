/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.models.LatitudeLongitude;
import com.ibm.whc.deid.providers.identifiers.IPAddressIdentifier;

public class RandomGeneratorsTest {
  @Test
  public void testLuhnGenerator() {
    String body = "402679993722";
    int digit = RandomGenerators.luhnCheckDigit(body);
    assertEquals(3, digit);

    body = "53305186243923";
    digit = RandomGenerators.luhnCheckDigit(body);
    assertEquals(8, digit);
  }

  @Test
  @Ignore
  public void testPerformanceRandomHostname() {
    int N = 1000000;

    long startMillis = System.currentTimeMillis();
    for (int i = 0; i < N; i++) {
      RandomGenerators.randomHostnameGenerator("ie.ibm.com", 2);
    }

    long diff = System.currentTimeMillis() - startMillis;
    System.out.println(String.format("%d operations took %d milliseconds (%f msec per op)", N, diff,
        (double) diff / N));
    // Assert test always should finish in less than 10 seconds
    assertTrue(diff < 10000);
  }

  @Test
  @Ignore
  public void testPerformanceRandomUsername() {
    int N = 1000000;

    long startMillis = System.currentTimeMillis();
    for (int i = 0; i < N; i++) {
      RandomGenerators.randomUsernameAndDomain(-1);
    }

    long diff = System.currentTimeMillis() - startMillis;
    System.out.println(String.format("%d operations took %d milliseconds (%f per op)", N, diff,
        (double) diff / N));
    // Assert test always should finish in less than 10 seconds
    assertTrue(diff < 10000);
  }

  @Test
  public void testRandomWithRange() throws Exception {

    int[] bases = {10, -10};

    for (int base : bases) {
      for (int i = 0; i < 100; i++) {
        int random = RandomGenerators.randomWithinRange(base, 10, 10);
        assertTrue(random >= (base - 10) && random <= (base + 10));

        random = RandomGenerators.randomWithinRange(base, 10, 0);
        assertTrue(random <= base && random >= (base - 10));

        random = RandomGenerators.randomWithinRange(base, 0, 10);
        assertTrue(random >= base && random <= (base + 10));
      }
    }
  }

  @Test
  public void testRandomIntervalWithinRangeWithPrecision() throws Exception {
    // lowerBound, upperBound
    int[] bounds = {2, 4, 0, 10, 1, 1, 50, 100, -5, -1, -2, 4, 2, 2,};
    for (int j = 0; j < bounds.length; j++) {
      int lowerBound = bounds[j];
      int upperBound = bounds[++j];
      for (int i = 0; i < 100; i++) {
        double random = RandomGenerators.randomWithinRangeWithPrecision(lowerBound, 0, 0, 0,
            upperBound - lowerBound);
        assertTrue(random >= (lowerBound) && random <= (upperBound + 1));
      }
    }
  }

  @Test
  public void testGenerateRandomCoordinates() throws Exception {
    double latitude = 90.0;
    double longitude = 180.0;

    LatitudeLongitude originalLatitudeLongitude = new LatitudeLongitude(latitude, longitude);

    for (int i = 0; i < 100; i++) {
      LatitudeLongitude randomLatitudeLongitude =
          RandomGenerators.generateRandomCoordinate(originalLatitudeLongitude, 100);

      assertTrue(randomLatitudeLongitude.getLatitude() >= -90.0);
      assertTrue(randomLatitudeLongitude.getLatitude() <= 90.0);
      assertTrue(randomLatitudeLongitude.getLongitude() >= -180.0);
      assertTrue(randomLatitudeLongitude.getLongitude() <= 180.0);

      assertFalse(originalLatitudeLongitude.equals(randomLatitudeLongitude));

      double distance =
          GeoUtils.latitudeLongitudeDistance(originalLatitudeLongitude, randomLatitudeLongitude);
      assertTrue(distance <= (100.0 + 0.5));
    }
  }

  @Test
  public void testRandomDirection() {
    int radius = 100;

    for (int i = 0; i < 1000; i++) {
      LatitudeLongitude original = RandomGenerators.generateRandomCoordinate();
      LatitudeLongitude randomCoordinate =
          RandomGenerators.generateRandomCoordinateRandomDirection(original, radius);
      double distance = GeoUtils.latitudeLongitudeDistance(original, randomCoordinate);
      assertEquals(100.0, distance, 0.1);
    }
  }

  @Test
  public void testGenerateRandomCoordinatesDonut() throws Exception {
    double latitude = 40.0;
    double longitude = 120.0;

    LatitudeLongitude originalLatitudeLongitude = new LatitudeLongitude(latitude, longitude);

    for (int i = 0; i < 100; i++) {
      LatitudeLongitude randomLatitudeLongitude =
          RandomGenerators.generateRandomCoordinate(originalLatitudeLongitude, 50, 100);

      assertTrue(randomLatitudeLongitude.getLatitude() >= -90.0);
      assertTrue(randomLatitudeLongitude.getLatitude() <= 90.0);
      assertTrue(randomLatitudeLongitude.getLongitude() >= -180.0);
      assertTrue(randomLatitudeLongitude.getLongitude() <= 180.0);

      assertFalse(originalLatitudeLongitude.equals(randomLatitudeLongitude));

      double distance =
          GeoUtils.latitudeLongitudeDistance(originalLatitudeLongitude, randomLatitudeLongitude);
      assertTrue(distance >= (50.0));
      assertTrue(distance <= (100.0 + 0.5));
    }
  }

  @Test
  public void testRandomHostname() throws Exception {

    String hostname = "1.2.3.4";
    String randomHostname = RandomGenerators.randomHostnameGenerator(hostname, 0);
    assertFalse(randomHostname.equals(hostname));
    assertTrue(new IPAddressIdentifier().isOfThisType(randomHostname));

    hostname = "www.nba.com";
    int randomizationOK = 0;

    for (int i = 0; i < 100; i++) {
      randomHostname = RandomGenerators.randomHostnameGenerator(hostname, 0);
      assertFalse(randomHostname.equals(hostname));

      if (!randomHostname.endsWith(".com")) {
        randomizationOK++;
      }
    }

    assertTrue(randomizationOK > 0);

    hostname = "www.nba.co.uk";

    for (int i = 0; i < 100; i++) {
      randomHostname = RandomGenerators.randomHostnameGenerator(hostname, 1);
      assertFalse(randomHostname.equals(hostname));
      assertTrue(randomHostname.endsWith(".co.uk"));
    }

    // check that hostname without TLD is processed
    hostname = "adasdasdad";
    randomHostname = RandomGenerators.randomHostnameGenerator(hostname, 0);
    assertFalse(randomHostname.equals(hostname));
  }

  @Test
  public void testRandomDate() throws Exception {
    DateTimeFormatter dateFormat =
        DateTimeFormatter.ofPattern("MM/dd/yyyy HH:mm:ss").withZone(ZoneOffset.UTC);
    String date = RandomGenerators.generateRandomDate(dateFormat);
    Date randomDate = Date.from(Instant.from(dateFormat.parse(date)));
    assertTrue(randomDate.before(new Date()));
  }

  @Test
  public void testRandomDate2() throws Exception {
    assertNotNull(RandomGenerators.generateRandomDate());
  }

  @Test
  public void testRandomReplacement() throws Exception {
    String initialString = "The quick brown fox jumps over the lazy dog 1234567890";
    String finalString = RandomGenerators.randomReplacement(initialString);
    assertEquals(initialString.length(), finalString.length());
    assertFalse(initialString.equalsIgnoreCase(finalString));
  }

  @Test
  public void testRandomUIDGenerator() throws Exception {
    String randomUID = RandomGenerators.randomUIDGenerator(10);
    assertNotNull(randomUID);
    assertEquals(10, randomUID.length());
  }

  @Test
  public void testRandomUIDGeneratorWithExclusion() throws Exception {
    char[] excludedChars =
        new char[] {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm'};
    String randomUID = RandomGenerators.randomUIDGenerator(100, excludedChars);
    assertNotNull(randomUID);
    assertEquals(100, randomUID.length());
    for (char excludedChar : excludedChars) {
      if (randomUID.indexOf(excludedChar) != -1) {
        fail();
      }
    }
  }

  @Test
  public void testRandomUIDGeneratorWithInclusion() throws Exception {
    char[] includedChars =
        new char[] {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm'};
    String randomUID = RandomGenerators.randomUIDGeneratorWithInclusions(100, includedChars);
    assertNotNull(randomUID);
    assertEquals(100, randomUID.length());
    String includedAsString = String.valueOf(includedChars);
    for (char randomChar : randomUID.toCharArray()) {
      if (includedAsString.indexOf(randomChar) == -1) {
        fail();
      }
    }
  }
}
