/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.utils.log;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

public class Messages {

  private static final String MY_FILE = "logCodes";
  private static ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(MY_FILE);

  public static String getMessage(String key, Object... params) throws MissingResourceException {
    if (params != null && params.length > 0) {
      return MessageFormat.format(RESOURCE_BUNDLE.getString(key), params);
    } else {
      return RESOURCE_BUNDLE.getString(key);
    }
  }
}
