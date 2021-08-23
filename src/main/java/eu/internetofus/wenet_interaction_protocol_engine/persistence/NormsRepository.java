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

import eu.internetofus.common.model.Model;
import eu.internetofus.common.model.ValidationErrorException;
import eu.internetofus.common.vertx.ModelsPageContext;
import eu.internetofus.common.vertx.QueryBuilder;
import eu.internetofus.common.vertx.Repository;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNorm;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNormsPage;
import io.vertx.codegen.annotations.GenIgnore;
import io.vertx.codegen.annotations.ProxyGen;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.serviceproxy.ServiceBinder;
import java.util.List;

/**
 * The service to manage the {@link PublishedNorm} on the database.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ProxyGen
public interface NormsRepository {

  /**
   * The address of this service.
   */
  String ADDRESS = "wenet_interaction_protocol_engine.persistence.norms";

  /**
   * Register this service.
   *
   * @param vertx   that contains the event bus to use.
   * @param pool    to create the database connections.
   * @param version of the schemas.
   *
   * @return the future that inform when the repository will be registered or not.
   */
  static Future<Void> register(final Vertx vertx, final MongoClient pool, final String version) {

    final var repository = new NormsRepositoryImpl(vertx, pool, version);
    new ServiceBinder(vertx).setAddress(NormsRepository.ADDRESS).register(NormsRepository.class, repository);
    return repository.migrateDocumentsToCurrentVersions();

  }

  /**
   * Create a proxy of the {@link NormsRepository}.
   *
   * @param vertx where the service has to be used.
   *
   * @return the norm.
   */
  static NormsRepository createProxy(final Vertx vertx) {

    return new NormsRepositoryVertxEBProxy(vertx, NormsRepository.ADDRESS);
  }

  /**
   * Search for the published norm with the specified identifier.
   *
   * @param id identifier of the norm to search.
   * @return the future search model.
   */
  @GenIgnore
  default Future<PublishedNorm> searchPublishedNorm(final String id) {

    final Promise<JsonObject> promise = Promise.promise();
    this.searchPublishedNorm(id, promise);
    return Model.fromFutureJsonObject(promise.future(), PublishedNorm.class);

  }

  /**
   * Search for the published norm with the specified identifier.
   *
   * @param id            identifier of the norm to search.
   * @param searchHandler handler to manage the search.
   */
  void searchPublishedNorm(String id, Handler<AsyncResult<JsonObject>> searchHandler);

  /**
   * Store a published norm.
   *
   * @param norm to store.
   *
   * @return the future stored norm.
   */
  @GenIgnore
  default Future<PublishedNorm> storePublishedNorm(final PublishedNorm norm) {

    final Promise<JsonObject> promise = Promise.promise();
    final var object = norm.toJsonObject();
    if (object == null) {

      return Future.failedFuture("The published norm can not converted to JSON.");

    } else {

      this.storePublishedNorm(object, promise);

    }

    return Model.fromFutureJsonObject(promise.future(), PublishedNorm.class);
  }

  /**
   * Store a published norm.
   *
   * @param norm         to store.
   * @param storeHandler handler to manage the search.
   */
  void storePublishedNorm(JsonObject norm, Handler<AsyncResult<JsonObject>> storeHandler);

  /**
   * Update a published norm.
   *
   * @param norm to update.
   *
   * @return the future update norm.
   */
  @GenIgnore
  default Future<Void> updatePublishedNorm(final PublishedNorm norm) {

    final Promise<Void> promise = Promise.promise();
    final var object = norm.toJsonObject();
    if (object == null) {

      return Future.failedFuture("The published norm can not converted to JSON.");

    } else {

      this.updatePublishedNorm(object, promise);

    }
    return promise.future();
  }

  /**
   * Update a published norm.
   *
   * @param norm          to update.
   * @param updateHandler handler to manage the update result.
   */
  void updatePublishedNorm(JsonObject norm, Handler<AsyncResult<Void>> updateHandler);

  /**
   * Delete a published norm.
   *
   * @param id            identifier of the norm to delete.
   * @param deleteHandler handler to manage the delete result.
   */
  void deletePublishedNorm(String id, Handler<AsyncResult<Void>> deleteHandler);

  /**
   * Delete a published norm.
   *
   * @param id identifier of the norm to delete.
   *
   * @return the future status of the delete action.
   */
  @GenIgnore
  default Future<Void> deletePublishedNorm(final String id) {

    final Promise<Void> promise = Promise.promise();
    this.deletePublishedNorm(id, promise);
    return promise.future();

  }

  /**
   * Create a query to obtain the norms that has the specified parameters.
   *
   * @param name        of the norms to return.
   * @param description of the norms to return.
   * @param keywords    of the norms to return.
   * @param publisherId identifier of the user that has published the norm.
   * @param publishFrom minimal deadline time stamp of the tasks to return.
   * @param publishTo   maximal deadline time stamp of the tasks to return.
   *
   * @return the query that will return the required norms.
   */
  static JsonObject createPublishedNormsPageQuery(final String name, final String description,
      final List<String> keywords, final String publisherId, final Long publishFrom, final Long publishTo) {

    return new QueryBuilder().withEqOrRegex("name", name).withEqOrRegex("description", description)
        .withEqOrRegex("keywords", keywords).withEqOrRegex("publisherId", publisherId)
        .withRange("_lastUpdateTs", publishFrom, publishTo).build();
  }

  /**
   * Create the components used to sort.
   *
   * @param order to use.
   *
   * @return the component that has to be used to sort the norms.
   *
   * @throws ValidationErrorException if the sort parameter is not valid.
   */
  static JsonObject createPublishedNormsPageSort(final List<String> order) throws ValidationErrorException {

    final var sort = Repository.queryParamToSort(order, "bad_order", (value) -> {

      switch (value) {
      case "name":
      case "description":
      case "keywords":
      case "publisherId":
        return value;
      case "publish":
      case "publishTime":
      case "publishedTime":
        return "_lastUpdateTs";
      default:
        return null;
      }

    });
    return sort;
  }

  /**
   * Retrieve the norms defined on the context.
   *
   * @param context that describe witch page want to obtain.
   *
   * @return the future search handler.
   */
  @GenIgnore
  default Future<PublishedNormsPage> retrievePublishedNormsPage(final ModelsPageContext context) {

    return this.retrievePublishedNormsPage(context.query, context.sort, context.offset, context.limit);

  }

  /**
   * Retrieve the norms defined on the context.
   *
   * @param query  to obtain the required norms.
   * @param sort   describe how has to be ordered the obtained norms.
   * @param offset the index of the first community to return.
   * @param limit  the number maximum of norms to return.
   *
   * @return the future search handler.
   */
  @GenIgnore
  default Future<PublishedNormsPage> retrievePublishedNormsPage(final JsonObject query, final JsonObject sort,
      final int offset, final int limit) {

    final Promise<JsonObject> promise = Promise.promise();
    this.retrievePublishedNormsPage(query, sort, offset, limit, promise);
    return Model.fromFutureJsonObject(promise.future(), PublishedNormsPage.class);

  }

  /**
   * Retrieve a page with some norms.
   *
   * @param query   to obtain the required norms.
   * @param sort    describe how has to be ordered the obtained norms.
   * @param offset  the index of the first community to return.
   * @param limit   the number maximum of norms to return.
   * @param handler to inform of the found norms.
   */
  void retrievePublishedNormsPage(JsonObject query, JsonObject sort, int offset, int limit,
      Handler<AsyncResult<JsonObject>> handler);

}
