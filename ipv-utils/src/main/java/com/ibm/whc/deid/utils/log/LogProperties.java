/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.utils.log;

import java.io.Serializable;

public class LogProperties implements Serializable {

  /**
   * 
   */
  private static final long serialVersionUID = -2403849757734224453L;
  private String localIpAddress;
  private String project;
  private String component;

  public String getLocalIpAddress() {
    return localIpAddress;
  }

  public void setLocalIpAddress(String localIpAddress) {
    this.localIpAddress = localIpAddress;
  }

  public String getProject() {
    return project;
  }

  public void setProject(String project) {
    this.project = project;
  }

  public String getComponent() {
    return component;
  }

  public void setComponent(String component) {
    this.component = component;
  }
}
