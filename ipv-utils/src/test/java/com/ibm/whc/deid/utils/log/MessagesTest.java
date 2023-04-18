/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.utils.log;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;

import org.junit.Test;

public class MessagesTest extends Messages {

  @Test
  public void getMessage() {

    String res = MessagesTest.getMessage("WPH1000I", "this is for unit test");
    assertThat(res, is("IPV-core information : \"this is for unit test\""));
  }
}
