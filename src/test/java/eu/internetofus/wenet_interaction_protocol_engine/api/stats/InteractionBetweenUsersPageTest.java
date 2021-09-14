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

package eu.internetofus.wenet_interaction_protocol_engine.api.stats;

import eu.internetofus.common.model.ModelTestCase;
import java.util.ArrayList;

/**
 * Test the {@link InteractionBetweenUsersPage}.
 *
 * @see InteractionBetweenUsersPage
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class InteractionBetweenUsersPageTest extends ModelTestCase<InteractionBetweenUsersPage> {

  /**
   * {@inheritDoc}
   */
  @Override
  public InteractionBetweenUsersPage createModelExample(final int index) {

    final var model = new InteractionBetweenUsersPage();
    model.offset = index;
    model.total = 100 + index;
    model.interactions = new ArrayList<>();
    model.interactions.add(new InteractionBetweenUsersTest().createModelExample(index - 1));
    model.interactions.add(new InteractionBetweenUsersTest().createModelExample(index));
    model.interactions.add(new InteractionBetweenUsersTest().createModelExample(index + 1));
    return model;
  }

}