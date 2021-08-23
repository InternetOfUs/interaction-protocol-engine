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
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNorm;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNormTest;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
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
 * Integration test over the {@link NormsRepository}.
 *
 * @see NormsRepository
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class NormsRepositoryIT {

  /**
   * Verify that can not found a published norm if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#searchPublishedNorm(String, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotFoundUndefinedPublishedNorm(final Vertx vertx, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).searchPublishedNorm("undefined norm identifier", testContext.failing(failed -> {
      testContext.completeNow();
    }));

  }

  /**
   * Verify that can not found a published norm object if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#searchPublishedNorm(String, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotFoundUndefinedPublishedNormObject(final Vertx vertx, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).searchPublishedNorm("undefined norm identifier", testContext.failing(failed -> {
      testContext.completeNow();
    }));

  }

  /**
   * Verify that can found a published norm.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#searchPublishedNorm(String)
   */
  @Test
  public void shouldFoundPublishedNorm(final Vertx vertx, final VertxTestContext testContext) {

    testContext.assertComplete(NormsRepository.createProxy(vertx).storePublishedNorm(new PublishedNorm()))
        .onSuccess(storedNorm -> {
          testContext.assertComplete(NormsRepository.createProxy(vertx).searchPublishedNorm(storedNorm.id)).onSuccess(

              foundNorm -> testContext.verify(() -> {
                assertThat(foundNorm).isEqualTo(storedNorm);
                testContext.completeNow();
              }));
        });

  }

  /**
   * Verify that can not store a published norm that can not be an object.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#storePublishedNorm(PublishedNorm)
   */
  @Test
  public void shouldNotStoreANormThatCanNotBeAnObject(final Vertx vertx, final VertxTestContext testContext) {

    final PublishedNorm norm = new PublishedNorm() {

      /**
       * {@inheritDoc}
       */
      @Override
      public JsonObject toJsonObject() {

        return null;
      }
    };
    norm.id = "undefined norm identifier";
    testContext.assertFailure(NormsRepository.createProxy(vertx).storePublishedNorm(norm))
        .onFailure(failed -> testContext.completeNow());

  }

  /**
   * Verify that can store a published norm.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#searchPublishedNorm(String, io.vertx.core.Handler)
   */
  @Test
  public void shouldStorePublishedNorm(final Vertx vertx, final VertxTestContext testContext) {

    final var norm = new PublishedNorm();
    norm._creationTs = 0;
    norm._lastUpdateTs = 1;
    testContext.assertComplete(NormsRepository.createProxy(vertx).storePublishedNorm(norm))
        .onSuccess(storedNorm -> testContext.verify(() -> {

          assertThat(storedNorm).isNotNull();
          assertThat(storedNorm.id).isNotEmpty();
          assertThat(storedNorm._creationTs).isEqualTo(0);
          assertThat(storedNorm._lastUpdateTs).isEqualTo(1);
          testContext.completeNow();
        }));

  }

  /**
   * Verify that can store a published norm object.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#storePublishedNorm(JsonObject, io.vertx.core.Handler)
   */
  @Test
  public void shouldStorePublishedNormObject(final Vertx vertx, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).storePublishedNorm(new JsonObject(),
        testContext.succeeding(storedNorm -> testContext.verify(() -> {

          assertThat(storedNorm).isNotNull();
          final var id = storedNorm.getString("id");
          assertThat(id).isNotEmpty();
          assertThat(storedNorm.containsKey("_creationTs")).isFalse();
          assertThat(storedNorm.containsKey("_lastUpdateTs")).isFalse();
          testContext.completeNow();
        })));

  }

  /**
   * Verify that can not update a published norm if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#updatePublishedNorm(PublishedNorm)
   */
  @Test
  public void shouldNotUpdateUndefinedPublishedNorm(final Vertx vertx, final VertxTestContext testContext) {

    final var norm = new PublishedNorm();
    norm.id = "undefined norm identifier";
    testContext.assertFailure(NormsRepository.createProxy(vertx).updatePublishedNorm(norm))
        .onFailure(failed -> testContext.completeNow());

  }

  /**
   * Verify that can not update a published norm if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#updatePublishedNorm(PublishedNorm)
   */
  @Test
  public void shouldNotUpdateANormThatCanNotBeAnObject(final Vertx vertx, final VertxTestContext testContext) {

    final PublishedNorm norm = new PublishedNorm() {

      /**
       * {@inheritDoc}
       */
      @Override
      public JsonObject toJsonObject() {

        return null;
      }
    };
    norm.id = "undefined norm identifier";
    testContext.assertFailure(NormsRepository.createProxy(vertx).updatePublishedNorm(norm))
        .onFailure(failed -> testContext.completeNow());

  }

  /**
   * Verify that can update a published norm.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#updatePublishedNorm(PublishedNorm)
   */
  @Test
  public void shouldUpdatePublishedNorm(final Vertx vertx, final VertxTestContext testContext) {

    final var norm = new PublishedNorm();
    testContext.assertComplete(NormsRepository.createProxy(vertx).storePublishedNorm(norm))
        .onSuccess(stored -> testContext.verify(() -> {

          final var update = new PublishedNormTest().createModelExample(23);
          update.id = stored.id;
          Thread.sleep(1000);
          testContext
              .assertComplete(NormsRepository.createProxy(vertx).updatePublishedNorm(update)
                  .compose(empty -> NormsRepository.createProxy(vertx).searchPublishedNorm(stored.id)))
              .onSuccess(foundNorm -> testContext.verify(() -> {

                assertThat(foundNorm._creationTs).isEqualTo(stored._creationTs);
                assertThat(foundNorm._lastUpdateTs).isGreaterThanOrEqualTo(stored._lastUpdateTs);
                update._creationTs = stored._creationTs;
                update._lastUpdateTs = foundNorm._lastUpdateTs;
                assertThat(foundNorm).isEqualTo(update);
                testContext.completeNow();
              }));

        }));

  }

  /**
   * Verify that update a defined published norm object.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
   */
  @Test
  public void shouldUpdatePublishedNormObject(final Vertx vertx, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).storePublishedNorm(new JsonObject().put("name", "Norm name"),
        testContext.succeeding(stored -> testContext.verify(() -> {

          final var id = stored.getString("id");
          final var update = new JsonObject().put("id", id).put("name", "New norm name");
          NormsRepository.createProxy(vertx).updatePublishedNorm(update,
              testContext.succeeding(empty -> testContext.verify(() -> {

                NormsRepository.createProxy(vertx).searchPublishedNorm(id,
                    testContext.succeeding(foundNorm -> testContext.verify(() -> {
                      stored.put("name", "New norm name");
                      assertThat(foundNorm).isEqualTo(stored);
                      testContext.completeNow();
                    })));
              })));

        })));

  }

  /**
   * Verify that can not delete a published norm if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#searchPublishedNorm(String, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotDeleteUndefinedPublishedNorm(final Vertx vertx, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).deletePublishedNorm("undefined norm identifier", testContext.failing(failed -> {
      testContext.completeNow();
    }));

  }

  /**
   * Verify that can delete a published norm.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
   */
  @Test
  public void shouldDeletePublishedNorm(final Vertx vertx, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).storePublishedNorm(new JsonObject(), testContext.succeeding(stored -> {

      final var id = stored.getString("id");
      NormsRepository.createProxy(vertx).deletePublishedNorm(id, testContext.succeeding(success -> {

        NormsRepository.createProxy(vertx).searchPublishedNorm(id, testContext.failing(search -> {

          testContext.completeNow();

        }));

      }));

    }));

  }

  /**
   *
   * Create some {@link PublishedNormTest#createModelExample(int)}.
   *
   * @param vertx           event bus to use.
   * @param testContext     context that executes the test.
   * @param change          function to modify the pattern before to store it.
   * @param max             number maximum of norms to create.
   * @param norms           list to add the created norms.
   * @param creationHandler that manage the creation.
   */
  public void storeSomePublishedNorms(final Vertx vertx, final VertxTestContext testContext,
      final Consumer<PublishedNorm> change, final int max, final List<PublishedNorm> norms,
      final Handler<AsyncResult<Void>> creationHandler) {

    if (norms.size() == max) {

      creationHandler.handle(Future.succeededFuture());

    } else {

      final var norm = new PublishedNormTest().createModelExample(norms.size());
      change.accept(norm);
      testContext.assertComplete(NormsRepository.createProxy(vertx).storePublishedNorm(norm)).onSuccess(stored -> {

        norms.add(stored);
        this.storeSomePublishedNorms(vertx, testContext, change, max, norms, creationHandler);
      });
    }

  }

  /**
   * Verify that can obtain the published norms by name.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#retrievePublishedNormsPage(JsonObject, JsonObject, int,
   *      int)
   */
  @Test
  public void shoulFoundPublishedNormPageByName(final Vertx vertx, final VertxTestContext testContext) {

    final var repository = NormsRepository.createProxy(vertx);
    final var name = UUID.randomUUID().toString();
    final var query = NormsRepository.createPublishedNormsPageQuery(name, null, null, null, null, null);
    final var sort = new JsonObject();
    testContext.assertComplete(repository.retrievePublishedNormsPage(query, sort, 0, 10))
        .onSuccess(found -> testContext.verify(() -> {

          assertThat(found).isNotNull();
          assertThat(found.total).isEqualTo(0l);
          assertThat(found.norms).isNull();
          final List<PublishedNorm> norms = new ArrayList<>();
          this.storeSomePublishedNorms(vertx, testContext, norm -> norm.name = name, 20, norms,
              testContext.succeeding(empty -> {

                testContext.assertComplete(repository.retrievePublishedNormsPage(query, sort, 0, 10))
                    .onSuccess(found2 -> testContext.verify(() -> {

                      assertThat(found2).isNotNull();
                      assertThat(found2.total).isEqualTo(20l);
                      assertThat(found2.norms).isEqualTo(norms.subList(0, 10));

                      testContext.assertComplete(repository.retrievePublishedNormsPage(query, sort, 5, 6))
                          .onSuccess(found3 -> testContext.verify(() -> {

                            assertThat(found3).isNotNull();
                            assertThat(found3.total).isEqualTo(20l);
                            assertThat(found3.norms).isEqualTo(norms.subList(5, 11));
                            testContext.completeNow();

                          }));

                    }));

              }));

        }));

  }

  /**
   * Verify that can obtain the published norms by description.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#retrievePublishedNormsPage(JsonObject, JsonObject, int,
   *      int)
   */
  @Test
  public void shoulFoundPublishedNormPageByDescription(final Vertx vertx, final VertxTestContext testContext) {

    final var repository = NormsRepository.createProxy(vertx);
    final var description = UUID.randomUUID().toString();
    final var query = NormsRepository.createPublishedNormsPageQuery(null, description, null, null, null, null);
    final var sort = new JsonObject();
    testContext.assertComplete(repository.retrievePublishedNormsPage(query, sort, 0, 10))
        .onSuccess(found -> testContext.verify(() -> {

          assertThat(found).isNotNull();
          assertThat(found.total).isEqualTo(0l);
          assertThat(found.norms).isNull();
          final List<PublishedNorm> norms = new ArrayList<>();
          this.storeSomePublishedNorms(vertx, testContext, norm -> norm.description = description, 20, norms,
              testContext.succeeding(empty -> {

                testContext.assertComplete(repository.retrievePublishedNormsPage(query, sort, 0, 10))
                    .onSuccess(found2 -> testContext.verify(() -> {

                      assertThat(found2).isNotNull();
                      assertThat(found2.total).isEqualTo(20l);
                      assertThat(found2.norms).isEqualTo(norms.subList(0, 10));

                      testContext.assertComplete(repository.retrievePublishedNormsPage(query, sort, 5, 6))
                          .onSuccess(found3 -> testContext.verify(() -> {

                            assertThat(found3).isNotNull();
                            assertThat(found3.total).isEqualTo(20l);
                            assertThat(found3.norms).isEqualTo(norms.subList(5, 11));
                            testContext.completeNow();

                          }));

                    }));

              }));

        }));

  }

}
