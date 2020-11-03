/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.sample;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class SampleClassTest {

  @Test
  public void getHelloWorld() {
    SampleClass test = new SampleClass();
    
    System.out.println(SampleClass.TestA);
    assertEquals("Hello World!", test.getHelloWorld());
  }

}
