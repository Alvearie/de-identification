/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.pruners;

import java.util.List;
import com.ibm.whc.deid.shared.pojo.masking.ReferableNode;

/**
 * Interface supported by classes that modify the structure and contents of JSON documents at a
 * document-level.
 */
public interface GlobalProcessor {

  public List<ReferableNode> processBatch(List<ReferableNode> batch);
}
