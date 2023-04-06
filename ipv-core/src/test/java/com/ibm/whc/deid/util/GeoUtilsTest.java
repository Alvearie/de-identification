/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.models.LatitudeLongitude;

public class GeoUtilsTest {

  @Test
  public void testXYZToLatlon() {
    double x = 3785510.99716482;
    double y = -425712.308930765;
    double z = 5098442.88234343;

    LatitudeLongitude latitudeLongitude = GeoUtils.xyzToLatlon(x, y, z);

    assertEquals(53.4185907, latitudeLongitude.getLatitude(), 0.0001);
    assertEquals(-6.416436, latitudeLongitude.getLongitude(), 0.0001);
  }

  @Test
  public void testLatlonToXYZ() {

    double latitude = 53.4185907;
    double longitude = -6.416436;

    XYZ xyz = GeoUtils.latlonToXYZ(latitude, longitude);

    assertEquals(3785510.99716482, xyz.getX(), 1.0);
    assertEquals(-425712.308930765, xyz.getY(), 1.0);
    assertEquals(5098442.88234343, xyz.getZ(), 1.0);
  }

  @Test
  public void testLatitudeLongitudeDistance() throws Exception {

    double distance = GeoUtils.latitudeLongitudeDistance(10.0, 10.0, 10.0, 10.0);
    assertTrue(distance == 0.0);

    distance = GeoUtils.latitudeLongitudeDistance(53.4185907, -6.4164366, 53.4162888, -6.4144412);
    assertTrue(distance >= 280.0 && distance <= 300.0);

    double distanceWest179 =
        GeoUtils.latitudeLongitudeDistance(53.4185907, -6.4164366, 53.4162888, -179.0);
    double distanceEast179 =
        GeoUtils.latitudeLongitudeDistance(53.4185907, -6.4164366, 53.4162888, 179.0);
    double distanceWest1799 =
        GeoUtils.latitudeLongitudeDistance(53.4185907, -6.4164366, 53.4162888, -179.99);
    double distanceEast1799 =
        GeoUtils.latitudeLongitudeDistance(53.4185907, -6.4164366, 53.4162888, 179.99);
    double distanceWest =
        GeoUtils.latitudeLongitudeDistance(53.4185907, -6.4164366, 53.4162888, -180.0);
    double distanceEast =
        GeoUtils.latitudeLongitudeDistance(53.4185907, -6.4164366, 53.4162888, 180.0);
    assertTrue(distanceWest179 < distanceWest1799);
    assertTrue(distanceWest1799 < distanceWest);
    assertTrue(distanceWest == distanceEast);
    assertTrue(distanceEast < distanceEast1799);
    assertTrue(distanceEast1799 < distanceEast179);
  }
}
