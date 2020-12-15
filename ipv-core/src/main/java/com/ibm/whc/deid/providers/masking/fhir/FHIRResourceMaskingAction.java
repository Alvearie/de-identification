/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import java.io.Serializable;
import java.util.Arrays;
import com.ibm.whc.deid.providers.masking.AbstractComplexMaskingProvider;
import com.ibm.whc.deid.providers.masking.MaskingProvider;

public class FHIRResourceMaskingAction implements Serializable {

  private static final long serialVersionUID = 4703951735249581684L;

  private final String fullPath;
  private final String path;
  private final String[] paths;

  private final MaskingProvider maskingProvider;
  private final AbstractComplexMaskingProvider abstractComplexMaskingProvider;

  public String getShortRuleName() {
    return path;
  }

  public String getFullRuleName() {
    return fullPath;
  }

  public String[] getPaths() {
    return paths;
  }

  public MaskingProvider getMaskingProvider() {
    return maskingProvider;
  }

  public AbstractComplexMaskingProvider getAbstractComplexMaskingProvider() {
    return abstractComplexMaskingProvider;
  }

  public FHIRResourceMaskingAction(String fullPath, String path, MaskingProvider maskingProvider,
      AbstractComplexMaskingProvider abstractComplexMaskingProvider) {
    this.fullPath = fullPath;
    this.path = path;
    int eqeqIndex = path.indexOf("==");
    if (eqeqIndex > 0) {
      if (this.path.startsWith("/")) {
        this.paths = path.substring(1, eqeqIndex).split("/");
      } else {
        this.paths = path.substring(0, eqeqIndex).split("/");
      }
      paths[paths.length - 1] = paths[paths.length - 1] + path.substring(eqeqIndex);
    } else {
      if (this.path.startsWith("/")) {
        this.paths = path.substring(1).split("/");
      } else {
        this.paths = path.split("/");
      }
    }
    this.maskingProvider = maskingProvider;
    this.abstractComplexMaskingProvider = abstractComplexMaskingProvider;
  }

  @Override
  public String toString() {
    return "FHIRResourceMaskingAction [fullPath=" + fullPath + ", path=" + path + ", paths="
        + Arrays.toString(paths) + ", maskingProvider=" + maskingProvider
        + ", abstractComplexMaskingProvider=" + abstractComplexMaskingProvider + "]";
  }
}
