/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.external.api.test;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;
import com.ibm.whc.deid.entry.api.DeidEntry;
import com.ibm.whc.deid.entry.api.DeidProcessor;

public class DeidEntryDriver {

  public static void main(String[] args) throws Exception {

    List<String> data = Files.readAllLines(new File(args[0]).toPath(), StandardCharsets.UTF_8);

    String config =
        new String(Files.readAllBytes(new File(args[1]).toPath()), StandardCharsets.UTF_8);

    DeidProcessor proc = DeidEntry.getDeidProcessor(config);
    
    List<String> out = proc.process(data);

    System.out.println("--------------------------------------------------------");
    for (String s : data) {
      System.out.println(s);
    }

    System.out.println("--------------------------------------------------------");
    for (String s : out) {
      System.out.println(s);
    }
  }
}
