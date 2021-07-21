/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.providers.masking.fhir.MaskingProviderBuilder;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;
import com.ibm.whc.deid.utils.log.LogCodes;

import scala.Tuple2;

public abstract class AbstractComplexMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -7270189743655406461L;

  public static final String DISABLE_TYPES_VALUE = "default";

  protected final Map<String, MaskingProviderBuilder> maskingProviderMap = new HashMap<>();

  protected String keyForType;

  protected String certificateId;

  protected MaskingProviderFactory maskingProviderFactory;

  protected String identifier;

  public AbstractComplexMaskingProvider() {
    super(null, null);
  }

  public AbstractComplexMaskingProvider(DeidMaskingConfig maskingConfiguration) {
    this.keyForType = maskingConfiguration.getJson().getMessageTypeKey();
    // if no Key is being passed into json.messageTypeKey then set to
    // default
    if (this.keyForType == null || this.keyForType.trim().isEmpty()) {
      this.keyForType = DISABLE_TYPES_VALUE;
    }
  }

  @Override
  public String mask(String identifier) {
    return null;
  }

  /**
   * Reads masking configuration to get all the resources
   *
   * @param resource
   * @return
   */
  protected final List<Tuple2<String, JsonNode>> maskResource(
      List<Tuple2<String, JsonNode>> resource) {

    Set<String> resourceTypes = new HashSet<>();
    List<MaskingProviderBuilder.MaskingResource> partitionedList = resource.stream().map(input -> {
      String resourceTypeName = "";
      if (!keyForType.equals(DISABLE_TYPES_VALUE)) {
        JsonNode resourceNode = input._2.get(keyForType);
        if (resourceNode != null && resourceNode.isValueNode() && !resourceNode.isNull()) {
          String resourceType = resourceNode.asText();
          resourceTypeName = resourceType.toLowerCase();
        }
      } else {
        resourceTypeName = DISABLE_TYPES_VALUE;
      }
      resourceTypes.add(resourceTypeName);
      return new MaskingProviderBuilder.MaskingResource(input._1(), input._2(), resourceTypeName);
    }).collect(Collectors.toList());

    List<MaskingProviderBuilder.MaskingResource> finishedList =
        new ArrayList<>(partitionedList.size());
    MaskingProviderBuilder maskingProvider;
    for (String resourceName : resourceTypes) {
      List<MaskingProviderBuilder.MaskingResource> maskingInList =
          partitionedList.stream().filter(input -> {
            return input.getResourceType().equals(resourceName);
          }).collect(Collectors.toList());
      maskingProvider = maskingProviderMap.get(resourceName);
      if (maskingProvider != null) {
        List<MaskingProviderBuilder.MaskingResource> maskingOutList =
            maskingProvider.orchestrateMasking(maskingInList);
        finishedList.addAll(maskingOutList);
      } else {
        finishedList.addAll(maskingInList);
      }
    }

    return finishedList.stream().map(record -> {
      return new Tuple2<String, JsonNode>(record.getIdentifier(), record.getJsonNode());
    }).collect(Collectors.toList());
  }

  /**
   * Given a list of input, return the masked value in the input matching the resource
   *
   * @param resources
   * @return
   */
  public List<Tuple2<String, JsonNode>> maskJsonNode(List<Tuple2<String, JsonNode>> resources) {
    List<Tuple2<String, JsonNode>> maskedValue = maskResource(resources);
    return maskedValue.stream().filter(input -> input._2() != null).collect(Collectors.toList());
  }

  /**
   * Given a masking configuration, return the masked JSON and audit trail record
   *
   * @param line
   * @param jobId
   * @return
   */
  public List<ReferableData> maskWithBatch(List<ReferableData> batch, String jobId) {

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();

    return maskJsonNode(batch.stream().map(input -> {
      try {
        return new Tuple2<String, JsonNode>(input.getIdentifier(),
            mapper.readTree(input.getData()));
      } catch (IOException e) {
        log.logError(LogCodes.WPH1017E, e, "maskWithBatch", input.getData());
      }
      return null;
    }).collect(Collectors.toList())).stream().map(input -> {
      try {
        return new ReferableData(input._1(), mapper.writeValueAsString(input._2()));
      } catch (JsonProcessingException e) {
        log.logError(LogCodes.WPH1013E, e);
      }
      return null;
    }).collect(Collectors.toList());
  }

  public String getIdentifier() {
    return identifier;
  }

  public void setIdentifier(String identifier) {
    this.identifier = identifier;
  }
}
