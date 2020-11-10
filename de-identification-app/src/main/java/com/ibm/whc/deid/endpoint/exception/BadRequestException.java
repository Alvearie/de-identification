/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.endpoint.exception;

/*
 * BadRequestException is a RuntimeException which is thrown when there is invalid request input
 */
public class BadRequestException extends RuntimeException {

  private static final long serialVersionUID = -2404841586238636381L;

  public BadRequestException(String message) {
    super(message);
  }

  public BadRequestException(String message, Throwable cause) {
    super(message, cause);
  }
}
