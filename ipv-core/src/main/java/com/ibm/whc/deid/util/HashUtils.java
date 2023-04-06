/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.nio.charset.Charset;
import java.util.UUID;


public class HashUtils {

  public static long longFromHash(String value) {
    if (value == null) {
      value = "";
    }

    long hash =
        UUID.nameUUIDFromBytes(value.getBytes(Charset.forName("UTF-8"))).getMostSignificantBits();
    return hash;

  }
}
