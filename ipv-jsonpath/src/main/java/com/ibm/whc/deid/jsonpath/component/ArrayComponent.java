/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.jsonpath.component;

import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.jsonpath.JSONPathException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ArrayComponent extends FieldComponent {

  /**
   * 
   */
  private static final long serialVersionUID = -1587378039298373900L;
  private static final Pattern pattern = Pattern.compile("(.+)\\[(.+)\\]");
  private final boolean star;
  private final int position;

  public ArrayComponent(String arrayElement) {
    if (null == arrayElement)
      throw new NullPointerException("arrayElement cannot be null");

    Matcher matcher = pattern.matcher(arrayElement);

    if (matcher.find()) {
      this.fieldName = matcher.group(1).trim();
      String elementId = matcher.group(2).trim();

      if ("*".equals(elementId)) {
        star = true;
        position = -1;
      } else {
        star = false;
        position = Integer.parseInt(matcher.group(2).trim(), 10);
      }
    } else {
      throw new IllegalArgumentException(arrayElement + " does not identify an array element");
    }
  }

  @Override
  public JsonNode apply(final JsonNode obj) throws JSONPathException {
    if (star)
      throw new RuntimeException("searching for * in an array: feature not supported");

    final JsonNode result = super.apply(obj);
    if (result.isArray() && result.size() > position) {
      return result.get(position);
    }

    throw new JSONPathException("Incompatible object");
  }
}
