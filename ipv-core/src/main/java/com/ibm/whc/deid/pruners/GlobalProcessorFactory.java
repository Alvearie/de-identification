/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.pruners;

/**
 * Class that constructs instances of the GlobalProcessor.
 */
public class GlobalProcessorFactory {

  public GlobalProcessor getGlobalProcessor() {
    return new DefaultGlobalProcessor();
  }
}
