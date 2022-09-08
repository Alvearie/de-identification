/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.entry.spark.udf;

public class DeIdUDFDriver {

  public static void main(String args[]) throws Exception {
    DeIdUDF udf = new DeIdUDF();
    String input = "{\"ENROLID\": \"123ABC\"}";
    String output = udf.call(input);
    System.out.println(input);
    System.out.println(output);
  }
}
