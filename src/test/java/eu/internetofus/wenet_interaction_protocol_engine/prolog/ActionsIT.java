/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */
package eu.internetofus.wenet_interaction_protocol_engine.prolog;

import java.util.ArrayList;
import java.util.List;

/**
 * Test the commons.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class ActionsIT extends AbstractConditionsITC {

  /**
   * {@inheritDoc}
   *
   * @return {@code 2} in any case.
   */
  @Override
  protected int numberOfUsersToCreate() {

    return 2;
  }

  /**
   * Return the conditions that are {@code true} and has to be test.
   *
   * @return the list of condition that has to be {@code true}.
   */
  @Override
  public List<String> getSucessConditionsToTest() {

    final List<String> actions = new ArrayList<>();

    actions.add("social_ranking(['1','2','3'],['1','2','3'])");

    return actions;

  }

}
