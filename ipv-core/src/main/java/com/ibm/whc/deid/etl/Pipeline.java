/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.etl;

import com.ibm.whc.deid.util.Tuple;
import java.util.ArrayList;
import java.util.List;

public class Pipeline {
  /** The Pipeline object list. */
  List<PipelineObject> pipelineObjectList;

  /**
   * From json string pipeline.
   *
   * @param jsonConfiguration the json configuration
   * @return the pipeline
   */
  public static Pipeline fromJSONString(String jsonConfiguration) {
    return new Pipeline();
  }

  /**
   * Instantiates a new Pipeline.
   *
   * @param objects the objects
   */
  public Pipeline(List<PipelineObject> objects) {
    this.pipelineObjectList = new ArrayList<>(objects);
  }

  /** Instantiates a new Pipeline. */
  public Pipeline() {
    this.pipelineObjectList = new ArrayList<>();
  }

  /**
   * Run string.
   *
   * @param input the input
   * @return the string
   */
  public String run(String input) {

    for (PipelineObject object : pipelineObjectList) {
      Tuple<String, Boolean> result = object.apply(input);
      if (!result.getSecond()) {
        break;
      }
      input = result.getFirst();
    }

    return input;
  }
}
