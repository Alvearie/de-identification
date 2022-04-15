/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import java.io.Serializable;
import java.util.Arrays;
import com.ibm.whc.deid.providers.masking.MaskingProvider;

public class FHIRResourceMaskingAction implements Serializable {

  private static final long serialVersionUID = 3577202668575530209L;

  private final String fullPath;
  private final String path;
  private final String[] paths;

  private final MaskingProvider maskingProvider;

  public String getShortRuleName() {
    return path;
  }

  public String getFullRuleName() {
    return fullPath;
  }

  public String[] getPaths() {
    // thread-safety - ensure changes to the returned array are not reflected in this object
    String[] pathsReturned = new String[paths.length];
    System.arraycopy(paths, 0, pathsReturned, 0, paths.length);
    return pathsReturned;
  }

  public MaskingProvider getMaskingProvider() {
    return maskingProvider;
  }

  public FHIRResourceMaskingAction(String fullPath, String path, MaskingProvider maskingProvider) {
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
  }

  @Override
  public String toString() {
    return "FHIRResourceMaskingAction [fullPath=" + fullPath + ", path=" + path + ", paths="
        + Arrays.toString(paths) + ", maskingProvider=" + maskingProvider + "]";
  }
}
