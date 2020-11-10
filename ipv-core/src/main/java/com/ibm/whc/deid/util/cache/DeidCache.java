/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util.cache;

public interface DeidCache {

  public String getCachedValue(String key);

  public void putCachedValue(String key, String val);

  public void dropCache();
}
