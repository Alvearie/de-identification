/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class HashUtils {
  private static final LogManager logger = LogManager.getInstance();

  public static Long longFromHash(String value) {
    return longFromHash(value, "SHA-256");
  }

  public static Long longFromHash(String value, String algorithm) {
    try {
      MessageDigest md = MessageDigest.getInstance(algorithm);

      if (value == null) {
        value = "";
      }

      md.update(value.getBytes());
      return new BigInteger(md.digest()).longValue();
    } catch (NoSuchAlgorithmException e) {
      logger.logError(LogCodes.WPH1013E, e);

      throw new Error("Impossible to retrieve an instance of " + algorithm);
    }
  }
}
