/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

public class LatLonToCartesianTranslator implements PointTranslator {
  private static Double R = 6378137.0;

  public void translate(double[] point) {
    Double latitude = point[0];
    Double longitude = point[1];
    Double lon_r = Math.toRadians(longitude);
    Double lat_r = Math.toRadians(latitude);

    Double x = R * Math.cos(lat_r) * Math.cos(lon_r);
    Double y = R * Math.cos(lat_r) * Math.sin(lon_r);
    Double z = R * Math.sin(lat_r);

    point[0] = x;
    point[1] = y;
    point[2] = z;
  }
}
