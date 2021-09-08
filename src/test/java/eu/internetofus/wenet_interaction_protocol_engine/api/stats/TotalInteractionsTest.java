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
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test the {@link TotalInteractions}
 *
 * @see TotalInteractions
 *
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class TotalInteractionsTest extends ModelTestCase<TotalInteractions> {

  /**
   * {@inheritDoc}
   */
  @Override
  public TotalInteractions createModelExample(final int index) {

    final var model = new TotalInteractions();
    model.sourceId = "source of " + index;
    model.targetId = "target of " + index;
    model.firstTs = (long) index;
    model.lastTs = (long) index + 10000;
    model.total = (long) index;
    return model;
  }

}
