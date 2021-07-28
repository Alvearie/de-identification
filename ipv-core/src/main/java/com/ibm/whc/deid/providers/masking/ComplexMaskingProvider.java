/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.util.List;
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;

/**
 * Interface supported by top-level masking processors.
 */
public interface ComplexMaskingProvider {

  public List<ReferableData> maskWithBatch(List<ReferableData> payloadData, String jobId);

}
