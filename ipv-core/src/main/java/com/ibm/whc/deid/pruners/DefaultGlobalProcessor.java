/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.pruners;

import java.util.List;
import com.ibm.whc.deid.shared.pojo.masking.ReferableNode;

/**
 * Class that modifies the structure and contents of JSON documents at a document-level.
 */
public class DefaultGlobalProcessor implements GlobalProcessor {

  @Override
  public List<ReferableNode> processBatch(List<ReferableNode> batch) {
    return batch;
  }
}
