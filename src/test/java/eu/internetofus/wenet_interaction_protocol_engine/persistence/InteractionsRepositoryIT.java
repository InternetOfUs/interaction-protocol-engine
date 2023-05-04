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

import eu.internetofus.common.components.StoreServices;
import eu.internetofus.common.components.interaction_protocol_engine.Interaction;
import eu.internetofus.common.components.interaction_protocol_engine.InteractionTest;
import eu.internetofus.common.vertx.ModelsPageContext;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.function.Consumer;
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

  /**
   * Create some {@link Interaction}.
   *
   * @param vertx        event bus to use.
   * @param testContext  context that executes the test.
   * @param change       function to modify the pattern before to store it.
   * @param max          number maximum of interactions to create.
   * @param interactions list to add the created interactions.
   *
   * @return the future creation result.
   */
  public Future<Void> storeSomeInteractions(final Vertx vertx, final VertxTestContext testContext,
      final Consumer<Interaction> change, final int max, final List<Interaction> interactions) {

    if (interactions.size() == max) {

      return Future.succeededFuture();

    } else {

      final var interaction = new InteractionTest().createModelExample(interactions.size());
      change.accept(interaction);
      return testContext.assertComplete(InteractionsRepository.createProxy(vertx).store(interaction))
          .compose(stored -> {
            interactions.add(stored);
            return this.storeSomeInteractions(vertx, testContext, change, max, interactions);
          });
    }

  }

  /**
   * Check that delete all the interactions with the specified user.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   */
  @Test
  public void shouldDeleteAllInteractionByUser(final Vertx vertx, final VertxTestContext testContext) {

    StoreServices.storeProfileExample(43, vertx, testContext).onSuccess(profile -> {
      final List<Interaction> interactions = new ArrayList<>();
      testContext.assertComplete(this.storeSomeInteractions(vertx, testContext, interaction -> {
        if (interactions.size() % 2 == 0) {

          for (var i = 0; i < 10; i++) {

            if (i % 3 == 0) {

              interaction.senderId = profile.id;

            } else if (i % 3 == 1) {

              interaction.receiverId = profile.id;
            }
          }
        }

      }, 20, interactions)).onSuccess(any -> {

        testContext.assertComplete(InteractionsRepository.createProxy(vertx).deleteAllInteractionByUser(profile.id))
            .onSuccess(empty -> this.assertDeletedInteractionsByUser(profile.id, interactions, vertx, testContext));
      });
    });

  }

  /**
   * Check that the interactions of an user has been removed.
   *
   * @param profileId    identifier of the user.
   * @param interactions where the interactions has to be removed.
   * @param vertx        event bus to use.
   * @param testContext  context that executes the test.
   */
  private void assertDeletedInteractionsByUser(final String profileId, final List<Interaction> interactions,
      final Vertx vertx, final VertxTestContext testContext) {

    if (interactions.isEmpty()) {

      testContext.completeNow();

    } else {

      final var expected = interactions.remove(0);
      final var context = new ModelsPageContext();
      context.limit = 0;
      context.offset = 100;
      context.query = new JsonObject().put("senderId", expected.senderId).put("receiverId", expected.receiverId);
      testContext.assertComplete(InteractionsRepository.createProxy(vertx).retrieveInteractionsPage(context))
          .onSuccess(page -> {

            testContext.verify(() -> {

              if (expected.senderId.equals(profileId) || expected.receiverId.equals(profileId)) {

                assertThat(page.total).isEqualTo(0);

              } else {

                assertThat(page.total).isGreaterThanOrEqualTo(1);
                assertThat(page.interactions).isNotNull().contains(expected);

              }

            });

            this.assertDeletedInteractionsByUser(profileId, interactions, vertx, testContext);

          });
    }
  }

  /**
   * Check that delete all the interactions with the specified task.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   */
  @Test
  public void shouldDeleteAllInteractionByTask(final Vertx vertx, final VertxTestContext testContext) {

    StoreServices.storeTaskExample(43, vertx, testContext).onSuccess(task -> {
      final List<Interaction> interactions = new ArrayList<>();
      testContext.assertComplete(this.storeSomeInteractions(vertx, testContext, interaction -> {
        if (interactions.size() % 2 == 0) {

          for (var i = 0; i < 10; i++) {

            if (i % 2 == 0) {

              interaction.taskId = task.id;
            }
          }
        }

      }, 20, interactions)).onSuccess(any -> {

        testContext.assertComplete(InteractionsRepository.createProxy(vertx).deleteAllInteractionByTask(task.id))
            .onSuccess(empty -> this.assertDeletedInteractionsByTask(task.id, interactions, vertx, testContext));
      });
    });

  }

  /**
   * Check that the interactions of an task has been removed.
   *
   * @param taskId       identifier of the task.
   * @param interactions where the interactions has to be removed.
   * @param vertx        event bus to use.
   * @param testContext  context that executes the test.
   */
  private void assertDeletedInteractionsByTask(final String taskId, final List<Interaction> interactions,
      final Vertx vertx, final VertxTestContext testContext) {

    if (interactions.isEmpty()) {

      testContext.completeNow();

    } else {

      final var expected = interactions.remove(0);
      final var context = new ModelsPageContext();
      context.limit = 0;
      context.offset = 100;
      context.query = new JsonObject().put("taskId", expected.taskId);
      testContext.assertComplete(InteractionsRepository.createProxy(vertx).retrieveInteractionsPage(context))
          .onSuccess(page -> {

            testContext.verify(() -> {

              if (expected.taskId.equals(taskId)) {

                assertThat(page.total).isEqualTo(0);

              } else {

                assertThat(page.total).isGreaterThanOrEqualTo(1);
                assertThat(page.interactions).isNotNull().contains(expected);

              }

            });

            this.assertDeletedInteractionsByTask(taskId, interactions, vertx, testContext);

          });
    }
  }

}
