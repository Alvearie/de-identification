/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;

public class SSNUS implements Serializable{
	/**
		 * 
		 */
	private static final long serialVersionUID = -2224663594305331778L;
private final String areaNumber;
  private final String group;
  private final String serialNumber;

  /**
   * Instantiates a new Ssnus.
   *
   * @param areaNumber the area number
   * @param group the group
   * @param serialNumber the serial number
   */
  public SSNUS(String areaNumber, String group, String serialNumber) {
    this.areaNumber = areaNumber;
    this.group = group;
    this.serialNumber = serialNumber;
  }

  /**
   * Gets area number.
   *
   * @return the area number
   */
  public String getAreaNumber() {
    return areaNumber;
  }

  /**
   * Gets group.
   *
   * @return the group
   */
  public String getGroup() {
    return group;
  }

  /**
   * Gets serial number.
   *
   * @return the serial number
   */
  public String getSerialNumber() {
    return serialNumber;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder(areaNumber);
    builder.append("-");
    builder.append(group);
    builder.append("-");
    builder.append(serialNumber);
    return builder.toString();
  }
}
