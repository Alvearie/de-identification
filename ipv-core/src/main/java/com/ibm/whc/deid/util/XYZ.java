/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

public class XYZ {
  private final double x;
  private final double y;
  private final double z;

  public double getX() {
    return x;
  }

  public double getY() {
    return y;
  }

  public double getZ() {
    return z;
  }

  public XYZ(double x, double y, double z) {
    this.x = x;
    this.y = y;
    this.z = z;
  }
}
