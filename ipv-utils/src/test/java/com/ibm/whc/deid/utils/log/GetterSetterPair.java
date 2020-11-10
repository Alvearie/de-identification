/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.utils.log;

import java.lang.reflect.Method;

public class GetterSetterPair {

  /** The get method. */
  private Method getter;

  /** The set method. */
  private Method setter;

  /**
   * Returns the get method.
   *
   * @return The get method.
   */
  public Method getGetter() {
    return getter;
  }

  /**
   * Returns the set method.
   *
   * @return The set method.
   */
  public Method getSetter() {
    return setter;
  }

  /**
   * Returns if this has a getter and setting method set.
   *
   * @return If this has a getter and setting method set.
   */
  public boolean hasGetterAndSetter() {
    return this.getter != null && this.setter != null;
  }

  /**
   * Sets the get Method.
   *
   * @param getter The get Method.
   */
  public void setGetter(Method getter) {
    this.getter = getter;
  }

  /**
   * Sets the set Method.
   *
   * @param setter The set Method.
   */
  public void setSetter(Method setter) {
    this.setter = setter;
  }
}
