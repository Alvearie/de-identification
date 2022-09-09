/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.entry.api;

import java.util.List;

/**
 * Interface to De-Identification functions accessed via Java calls from a parent Java process.
 * 
 * <p>
 * Implementations of this interface are thread-safe.
 */
public interface DeidProcessor {

  /**
   * Applies privacy protection to a list of documents as per the configuration used when the
   * DeidProcessor instance was created.
   * 
   * @param input one or more documents
   * 
   * @return the documents modified with the output of the relevant privacy providers
   */
  public List<String> process(List<String> input);
}
