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

package eu.internetofus.wenet_interaction_protocol_engine.persistence;

import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.api.interactions.InteractionTest;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxTestContext;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration test over the {@link InteractionsRepository}.
 *
 * @see InteractionsRepository
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class InteractionsRepositoryIT {

  /**
   * Should not post bad interaction.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   */
  @Test
  public void shouldStoreInteraction(final Vertx vertx, final VertxTestContext testContext) {

    final var model = new InteractionTest().createModelExample(1);
    InteractionsRepository.createProxy(vertx).store(model)
        .onComplete(testContext.succeeding(stored -> testContext.verify(() -> {

          assertThat(stored).isEqualTo(model);
          testContext.completeNow();

        })));

  }

  /**
   * Should not post bad interaction.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   */
  @Test
  public void shouldReturnEmptyPageBecauseNoInteractionMatchQuery(final Vertx vertx,
      final VertxTestContext testContext) {

    final var transactionLabel = UUID.randomUUID().toString();
    final var query = InteractionsRepository.createInteractionsPageQuery(null, null, null, null, null, null, null,
        transactionLabel, null, null, null, null, null, null);
    InteractionsRepository.createProxy(vertx).retrieveInteractionsPage(query, null, 0, 100)
        .onComplete(testContext.succeeding(page -> testContext.verify(() -> {

          assertThat(page.total).isEqualTo(0);
          assertThat(page.interactions).isNullOrEmpty();
          testContext.completeNow();

        })));

  }

  /**
   * Should not post bad interaction.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   */
  @Test
  public void shouldFailDeleteBecauseNoInteractionMatchQuery(final Vertx vertx, final VertxTestContext testContext) {

    final var transactionLabel = UUID.randomUUID().toString();
    final var query = InteractionsRepository.createInteractionsPageQuery(null, null, null, null, null, null, null,
        transactionLabel, null, null, null, null, null, null);
    InteractionsRepository.createProxy(vertx).deleteInteractions(query)
        .onComplete(testContext.failing(any -> testContext.completeNow()));

  }

  /**
   * Should store an interaction, retrieve it, delete it, no retrieve after and
   * fail to try to remove again.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   */
  @Test
  public void shouldStoreRetrieveDeleteEmptyRetrieveAndFailDalete(final Vertx vertx,
      final VertxTestContext testContext) {

    final var model = new InteractionTest().createModelExample(1);
    model.transactionLabel = UUID.randomUUID().toString();
    InteractionsRepository.createProxy(vertx).store(model)
        .onComplete(testContext.succeeding(stored -> testContext.verify(() -> {

          assertThat(stored).isEqualTo(model);
          final var query = InteractionsRepository.createInteractionsPageQuery(null, null, null, null, null, null, null,
              model.transactionLabel, null, null, null, null, null, null);
          InteractionsRepository.createProxy(vertx).retrieveInteractionsPage(query, null, 0, 100)
              .onComplete(testContext.succeeding(page -> testContext.verify(() -> {

                assertThat(page.total).isEqualTo(1L);
                assertThat(page.interactions).hasSize(1).contains(model);
                InteractionsRepository.createProxy(vertx).deleteInteractions(query)
                    .onComplete(testContext.succeeding(any -> {

                      InteractionsRepository.createProxy(vertx).retrieveInteractionsPage(query, null, 0, 100)
                          .onComplete(testContext.succeeding(page2 -> testContext.verify(() -> {

                            assertThat(page2.total).isEqualTo(0);
                            assertThat(page2.interactions).isNullOrEmpty();
                            InteractionsRepository.createProxy(vertx).deleteInteractions(query)
                                .onComplete(testContext.failing(any2 -> testContext.completeNow()));

                          })));

                    }));

              })));

        })));

  }

}
