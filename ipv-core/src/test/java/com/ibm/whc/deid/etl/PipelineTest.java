/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.etl;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.ibm.whc.deid.util.Tuple;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;

public class PipelineTest {
  private static final String TEST_INPUT = "test_input";
  private static final String TEST_OUTPUT_FALSE = "test_output_false";
  private static final String TEST_OUTPUT_TRUE = "test_output_true";
  private static final Tuple<String, Boolean> falseResult = new Tuple<>(TEST_OUTPUT_FALSE, false);
  private static final Tuple<String, Boolean> trueResult = new Tuple<>(TEST_OUTPUT_TRUE, true);
  private PipelineObject pipelineObjectFalseResultMock;
  private PipelineObject pipelineObjectTrueResultMock;

  @Before
  public void setUp() throws Exception {
    pipelineObjectFalseResultMock = mock(PipelineObject.class);
    when(pipelineObjectFalseResultMock.apply(anyString())).thenReturn(falseResult);
    pipelineObjectTrueResultMock = mock(PipelineObject.class);
    when(pipelineObjectTrueResultMock.apply(anyString())).thenReturn(trueResult);
  }

  @Test
  public void testPipelineEmpty() throws Exception {

    Pipeline pipeline = Pipeline.fromJSONString(null);
    String output = pipeline.run(TEST_INPUT);
    assertEquals(output, TEST_INPUT);
  }

  @Test
  public void testPipelineFalseThenTrue() throws Exception {

    Pipeline pipeline =
        new Pipeline(Arrays.asList(pipelineObjectFalseResultMock, pipelineObjectTrueResultMock));
    String output = pipeline.run(TEST_INPUT);
    assertEquals(output, TEST_INPUT);
  }

  @Test
  public void testPipelineTrueThenFalse() throws Exception {

    Pipeline pipeline =
        new Pipeline(Arrays.asList(pipelineObjectTrueResultMock, pipelineObjectFalseResultMock));
    String output = pipeline.run(TEST_INPUT);
    assertEquals(output, TEST_OUTPUT_TRUE);
  }
}
