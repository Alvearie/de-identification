/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.models.LatitudeLongitude;
import com.ibm.whc.deid.util.GeoUtils;
import org.junit.Test;

public class LatitudeLongitudeIdentifierTest {
  @Test
  public void testIsOfThisType() throws Exception {

    LatitudeLongitudeIdentifier identifier = new LatitudeLongitudeIdentifier();

    String[] validLatitudeLongitudes = {"12:30'23.256547S 12:30'23.256547E",
        "12:12:12.2246N 12:12:12.2246W", "N90.00.00 E180.00.00", "N90.00.00,E180.00.00",
        "S34.59.33 W179.59.59", "N00.00.00 W000.00.00", "1.3653974,103.7474993"};

    for (String latitudeLongitude : validLatitudeLongitudes) {
      System.out.println(latitudeLongitude);
      assertTrue(identifier.isOfThisType(latitudeLongitude));
    }
  }

  @Test
  public void testIsNotOfThisType() throws Exception {
    LatitudeLongitudeIdentifier identifier = new LatitudeLongitudeIdentifier();

    String[] invalidLatitudeLongitudes = {"12:12:12.223546\"N", "12:12:12.2246N", "15:17:6\"S",
        "12°30'23.256547\"S", "12°30'23.256547S", "N91.00.00 E181.00.00", "Z34.59.33 W179.59.59",
        "N00.00.00 W181.00.00", "12.2225", "15.25.257S", "51° 31.7' N", "AA:BB:CC.DDS",
        "153.418596,-6.4163855", "53.418596,-186.4163855"};

    for (String latitudeLongitude : invalidLatitudeLongitudes) {
      assertFalse(identifier.isOfThisType(latitudeLongitude));
    }
  }

  @Test
  public void testParseCompassFormat() {
    LatitudeLongitudeIdentifier identifier = new LatitudeLongitudeIdentifier();
    String coords = "12:30'23.256547S 12:30'23.256547E";

    LatitudeLongitude latitudeLongitude = identifier.parseCompassFormat(coords);
    assertTrue(latitudeLongitude != null);
    assertEquals(latitudeLongitude.getLatitude(),
        -GeoUtils.degreesToDecimal(12.0, 30.0, 23.256547), 0.0);
    assertEquals(latitudeLongitude.getLongitude(),
        GeoUtils.degreesToDecimal(12.0, 30.0, 23.256547), 0.0);

    coords = "S34.59.33 W179.59.59";
    latitudeLongitude = identifier.parseCompassFormat(coords);
    assertTrue(latitudeLongitude != null);
    assertEquals(latitudeLongitude.getLatitude(), -GeoUtils.degreesToDecimal(34.0, 59.0, 33.0),
        0.0);
    assertEquals(latitudeLongitude.getLongitude(), -GeoUtils.degreesToDecimal(179.0, 59.0, 59.0),
        0.0);

    coords = "S90.00.00 W179.59.59";
    latitudeLongitude = identifier.parseCompassFormat(coords);
    assertNotNull(latitudeLongitude);
    assertEquals(latitudeLongitude.getLatitude(), -GeoUtils.degreesToDecimal(90.0, 0.0, 0.0), 0.0);
    assertEquals(latitudeLongitude.getLongitude(), -GeoUtils.degreesToDecimal(179.0, 59.0, 59.0),
        0.0);
  }
}
