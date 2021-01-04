/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.shared.pojo.config.masking.GeneralizeMaskingProviderConfig;
import com.ibm.whc.deid.utils.log.LogCodes;

public class GeneralizeMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 8003315754342350747L;

  private static final String GENERALIZE_MASK_RULESET = "generalize.mask.ruleSet";
  private static final String JSON_TARGETVALUE_TAG = "targetValue";
  private static final String JSON_SOURCE_VALUE_IN_TAG = "sourceValueIn";
  private static final String JSON_SOURCE_VALUE_NOTIN_TAG = "sourceValueNotIn";

  private final String generalizeMaskRuleSet;
  private final List<GeneralizeRule> generalizeRuleSet;

  /**
   * Instantiates a new Generalize masking provider.
   *
   * <p>
   * (1) For all values of a data element which do not belong in set A, replace these values with
   * "Other". E.g., all patients' languages that are not listed as Spanish or English, are mapped to
   * "Other".
   *
   * <p>
   * (2) For all values of a data element which belong in set A, replace these values with a
   * specified value. (Creation of more general categories through the combination of rarer traits
   * into more general traits)
   *
   * <p>
   * Some examples below: Race: Combine specific races into a larger category of race. For example,
   * Vietnamese, Thai, and Japanese will be combined into a category of "Asian".
   *
   * <p>
   * Religion: Combine lesser practiced religions into a larger category of religion. For example,
   * Daoism, Shinto, and Confucianism will be combined into "Eastern Asian Religions".
   */
  public GeneralizeMaskingProvider() {
    this(new GeneralizeMaskingProviderConfig());
  }

  public GeneralizeMaskingProvider(GeneralizeMaskingProviderConfig configuration) {
    this.generalizeMaskRuleSet = configuration.getMaskRuleSet();
    this.generalizeRuleSet =
        this.generalizeMaskRuleSet != null ? parseMaskRuleSet(generalizeMaskRuleSet)
            : new ArrayList<>();
  }

  private List<GeneralizeRule> parseMaskRuleSet(String ruleSetStr) {
    List<GeneralizeRule> rulesetList = new ArrayList<>();
    try {
      boolean parsingError = false;
      ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
      JsonNode ruleSetNode = mapper.readTree(ruleSetStr);
      if (ruleSetNode != null && !ruleSetNode.isMissingNode()) {

        for (JsonNode ruleNode : ruleSetNode) {
          /*
           * Validate the rule has a targetValue node, and also has either sourceValueIn or
           * valueSourceNotIn, but not both. Currently, must specify either sourceValueIn or
           * sourceValueNotIn but not both.
           */
          if (!ruleNode.has(JSON_TARGETVALUE_TAG)
              || (!ruleNode.has(JSON_SOURCE_VALUE_IN_TAG)
                  && !ruleNode.has(JSON_SOURCE_VALUE_NOTIN_TAG))
              || (ruleNode.has(JSON_SOURCE_VALUE_IN_TAG)
                  && ruleNode.has(JSON_SOURCE_VALUE_NOTIN_TAG))) {
            parsingError = true;
            break;
          }
          JsonNode targetValueNode = ruleNode.path(JSON_TARGETVALUE_TAG);
          String targetValue = targetValueNode.asText();

          // System.out.println("=======> rule: " +
          // ruleNode.toString());
          // System.out.println("=======> JSON_TARGETVALUE_TAG: " +
          // ruleNode.path(JSON_TARGETVALUE_TAG));
          GeneralizeRule ruleSet = new GeneralizeRule(targetValue);

          // System.out.println("=======> JSON_SOURCE_VALUE_IN_TAG: "
          // + ruleNode.path(JSON_SOURCE_VALUE_IN_TAG));
          // System.out.println("=======> JSON_SOURCE_VALUE_NOTIN_TAG:
          // "
          // + ruleNode.path(JSON_SOURCE_VALUE_NOTIN_TAG));

          JsonNode sourceValueInList = ruleNode.path(JSON_SOURCE_VALUE_IN_TAG);
          JsonNode sourceValueNotInList = ruleNode.path(JSON_SOURCE_VALUE_NOTIN_TAG);

          if (ruleNode.has(JSON_SOURCE_VALUE_IN_TAG)) {
            ruleSet.setLogicalNegation(false);
            for (JsonNode sourceValueIn : sourceValueInList) {
              String value = sourceValueIn.asText();
              ruleSet.getValueSet().add(value);
              // System.out.println("=======> sourceValueIn: " +
              // value);

            }
          } else {
            // sourceValueNotIn node:
            ruleSet.setLogicalNegation(true);
            for (JsonNode sourceValueNotIn : sourceValueNotInList) {
              String value = sourceValueNotIn.asText();
              ruleSet.getValueSet().add(value);
              // System.out.println("=======> sourceValueNotIn: "
              // + value);
            }
          }
          rulesetList.add(ruleSet);
        }
      } else {
        parsingError = true;
      }
      if (parsingError) {
        throw new RuntimeException();
      }
    } catch (Exception e) {
      String message = GENERALIZE_MASK_RULESET + ", ruleSet: " + this.generalizeMaskRuleSet;
      log.logError(LogCodes.WPH2011E, e, message);
      throw new RuntimeException(message);
    }

    return rulesetList;
  }

  /**
   * Masks the specified identifier to generalize categories based on a configured rule set.
   *
   * @param identifier
   * @return mask value
   */
  @Override
  public String mask(String identifier) {
    // If none of the rules apply, we return the original value.
    String maskValue = identifier;
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }
    if (this.generalizeMaskRuleSet == null) {
      // No masking rules provided.
      return null;
    }

    for (GeneralizeRule rule : generalizeRuleSet) {
      if (rule.getLogicalNegation()) {
        // We are processing a negated logic: the identifier is
        // replaced if it is not in the list of values defined.
        if (!rule.getValueSet().contains(identifier)) {
          maskValue = rule.getCategory();
          break; // we are done
        }
        // continue to the next rule. If there are no more rules,
        // then identifier is not replaced.
      } else {
        // We are processing the positive logic, the identifier is
        // replaced if it is in the list of values defined.
        if (rule.getValueSet().contains(identifier)) {
          maskValue = rule.getCategory();
          break; // we are done
        } else if (rule.getValueSet().size() == 1 && rule.getValueSet().contains("*")) {
          // this is the case where the value is replaced with
          // default (e.g., Other).
          maskValue = rule.getCategory();
          break; // we are done
        }
        // continue to the next rule. If there are no more rules,
        // then identifier is not replaced.
      }
    }

    // System.out.println("***=======>identifier: " + identifier +
    // ", mask value: " + maskValue);

    return maskValue;
  }

  private class GeneralizeRule implements Serializable {
    /** */
    private static final long serialVersionUID = -6537320477488404490L;

    Boolean logicalNegation;
    String category;
    Set<String> valueSet;

    GeneralizeRule(String category) {
      this.category = category;
      this.logicalNegation = false; // default to false
      valueSet = new HashSet<>();
    }

    public void setLogicalNegation(Boolean logicalNegation) {
      this.logicalNegation = logicalNegation;
    }

    public Boolean getLogicalNegation() {
      return logicalNegation;
    }

    public String getCategory() {
      return category;
    }

    public Set<String> getValueSet() {
      return valueSet;
    }
  }
}
